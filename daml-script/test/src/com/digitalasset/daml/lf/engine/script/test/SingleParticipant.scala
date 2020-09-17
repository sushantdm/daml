// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.engine.script.test

import java.io.File
import java.time.Duration

import com.daml.daml_lf_dev.DamlLf
import com.daml.lf.archive.{Dar, DarReader, Decode}
import com.daml.lf.data.Ref.{Party => LedgerParty, _}
import com.daml.lf.data.Time.Timestamp
import com.daml.lf.data.{FrontStack, FrontStackCons, Numeric}
import com.daml.lf.engine.script._
import com.daml.lf.language.Ast._
import com.daml.lf.speedy.SValue._
import scalaz.syntax.traverse._
import spray.json._

case class Config(
    ledgerPort: Int,
    darPath: File,
    // 1.dev DAR
    devDarPath: File,
    wallclockTime: Boolean,
    // We use the presence of a root CA as a proxy for whether to enable TLS or not.
    rootCa: Option[File],
)


case class SetTime(dar: Dar[(PackageId, Package)], runner: TestRunner) {
  val scriptId = Identifier(dar.main._1, QualifiedName.assertFromString("ScriptTest:testSetTime"))
  def runTests() = {
    runner.genericTest(
      "tTime",
      scriptId,
      None,
      result =>
        result match {
          case SRecord(_, _, vals) if vals.size == 2 =>
            for {
              t0 <- TestRunner.assertSTimestamp(vals.get(0))
              t1 <- TestRunner.assertSTimestamp(vals.get(1))
              _ <- TestRunner
                .assertEqual(t0, Timestamp.assertFromString("1970-01-01T00:00:00Z"), "t0")
              _ <- TestRunner
                .assertEqual(t1, Timestamp.assertFromString("2000-02-02T00:01:02Z"), "t1")
            } yield ()
          case v => Left(s"Expected Tuple2 but got $v")
      }
    )
  }
}

case class Sleep(dar: Dar[(PackageId, Package)], runner: TestRunner) {
  val scriptId = Identifier(dar.main._1, QualifiedName.assertFromString("ScriptTest:sleepTest"))
  def runTests() = {
    runner.genericTest(
      "Sleep",
      scriptId,
      None, {
        case SRecord(_, _, vals) if vals.size == 3 =>
          for {
            t0 <- TestRunner.assertSTimestamp(vals.get(0))
            t1 <- TestRunner.assertSTimestamp(vals.get(1))
            t2 <- TestRunner.assertSTimestamp(vals.get(2))
            _ <- if (Duration
                .between(t0.toInstant, t1.toInstant)
                .compareTo(Duration.ofSeconds(1)) < 0 && runner.wallclockTime)
              Left(s"Difference between $t0 and $t1 should be more than 1 second")
            else Right(())
            _ <- if (Duration
                .between(t1.toInstant, t2.toInstant)
                .compareTo(Duration.ofSeconds(2)) < 0 && runner.wallclockTime)
              Left(s"Difference between $t1 and $t2 should be more than 2 seconds")
            else Right(())
          } yield ()
        case v => Left(s"Expected SUnit but got $v")
      }
    )
  }
}

case class PartyIdHintTest(dar: Dar[(PackageId, Package)], runner: TestRunner) {
  val scriptId =
    Identifier(dar.main._1, QualifiedName.assertFromString("ScriptTest:partyIdHintTest"))
  def runTests() = {
    runner.genericTest(
      "PartyIdHint",
      scriptId,
      None, {
        case SRecord(_, _, vals) if vals.size == 2 =>
          for {
            _ <- TestRunner.assertEqual(
              vals.get(0),
              SParty(LedgerParty.assertFromString("carol")),
              "Accept party id hint")
            _ <- TestRunner.assertEqual(
              vals.get(1),
              SParty(LedgerParty.assertFromString("dan")),
              "Accept party id hint")
          } yield ()
      }
    )
  }
}

case class ListKnownParties(dar: Dar[(PackageId, Package)], runner: TestRunner) {
  val scriptId =
    Identifier(dar.main._1, QualifiedName.assertFromString("ScriptTest:listKnownPartiesTest"))
  def runTests() = {
    runner.genericTest(
      "ListKnownParties",
      scriptId,
      None, {
        case SRecord(_, _, vals) if vals.size == 2 =>
          for {
            newPartyDetails <- vals.get(0) match {
              case SList(FrontStackCons(SRecord(_, _, x), FrontStack())) => Right(x)
              case v => Left(s"Exppected list with one element but got $v")
            }
            _ <- TestRunner.assertEqual(newPartyDetails.get(0), vals.get(1), "new party")
            _ <- TestRunner
              .assertEqual(newPartyDetails.get(1), SOptional(Some(SText("myparty"))), "displayName")
            _ <- TestRunner.assertEqual(newPartyDetails.get(2), SBool(true), "isLocal")
          } yield ()
      }
    )
  }
}

case class TestStack(dar: Dar[(PackageId, Package)], runner: TestRunner) {
  val scriptId =
    Identifier(dar.main._1, QualifiedName.assertFromString("ScriptTest:testStack"))
  def runTests() = {
    runner.genericTest(
      "testStack",
      scriptId,
      None,
      // We only want to check that this does not crash so the check here is trivial.
      v => TestRunner.assertEqual(v, SUnit, "Script result")
    )
  }
}

case class TestMaxInboundMessageSize(dar: Dar[(PackageId, Package)], runner: TestRunner) {
  val scriptId =
    Identifier(dar.main._1, QualifiedName.assertFromString("ScriptTest:testMaxInboundMessageSize"))
  def runTests(): Unit = {
    runner.genericTest(
      "MaxInboundMessageSize",
      scriptId,
      None, {
        case SUnit => Right(())
        case v => Left(s"Expected SUnit but got $v")
      },
      maxInboundMessageSize = RunnerConfig.DefaultMaxInboundMessageSize * 10,
    )
  }
}

// Runs the example from the docs to make sure it doesn’t produce a runtime error.
case class ScriptExample(dar: Dar[(PackageId, Package)], runner: TestRunner) {
  val scriptId = Identifier(dar.main._1, QualifiedName.assertFromString("ScriptExample:test"))
  def runTests(): Unit = {
    runner.genericTest(
      "ScriptExample",
      scriptId,
      None, {
        case SUnit => Right(())
        case v => Left(s"Expected SUnit but got $v")
      }
    )
  }
}

case class TraceOrder(dar: Dar[(PackageId, Package)], runner: TestRunner) {
  val scriptId = Identifier(dar.main._1, QualifiedName.assertFromString("ScriptTest:traceOrder"))
  def traceMsg(msg: String) = s"""[DA.Internal.Prelude:540]: "$msg""""
  def runTests(): Unit = {
    runner.genericTest(
      "traceOrder",
      scriptId,
      None, {
        case SUnit => Right(())
        case v => Left(s"Expected SUnit but got $v")
      },
      Some(Seq(traceMsg("abc"), traceMsg("def"), traceMsg("abc"), traceMsg("def")))
    )
  }
}

case class TestContractId(dar: Dar[(PackageId, Package)], runner: TestRunner) {
  val scriptId =
    Identifier(dar.main._1, QualifiedName.assertFromString("TestContractId:testContractId"))
  def runTests(): Unit = {
    runner.genericTest(
      "testContractId",
      scriptId,
      None, {
        case SRecord(_, _, vals) if vals.size == 2 => {
          (vals.get(0), vals.get(1)) match {
            case (SContractId(cid), SText(t)) =>
              TestRunner.assertEqual(t, cid.coid, "contract ids")
            case (a, b) =>
              Left(s"Expected SContractId, SText but got $a, $b")
          }
        }
        case v => Left(s"Expected Tuple2 but got $v")
      }
    )
  }
}

case class TestQueryContractId(dar: Dar[(PackageId, Package)], runner: TestRunner) {
  val scriptId =
    Identifier(dar.main._1, QualifiedName.assertFromString("ScriptTest:testQueryContractId"))
  def runTests(): Unit = {
    runner.genericTest(
      "testQueryContractId",
      scriptId,
      None, {
        // We test with assertions in the test
        case SUnit => Right(())
        case v => Left(s"Expected SUnit but got $v")
      }
    )
  }
}

case class TestQueryContractKey(dar: Dar[(PackageId, Package)], runner: TestRunner) {
  val scriptId =
    Identifier(dar.main._1, QualifiedName.assertFromString("ScriptTest:testQueryContractKey"))
  def runTests(): Unit = {
    runner.genericTest(
      "testQueryContractKey",
      scriptId,
      None, {
        // We test with assertions in the test
        case SUnit => Right(())
        case v => Left(s"Expected SUnit but got $v")
      }
    )
  }
}

object SingleParticipant {

  private val configParser = new scopt.OptionParser[Config]("daml_script_test") {
    head("daml_script_test")

    opt[Int]("target-port")
      .required()
      .action((p, c) => c.copy(ledgerPort = p))

    arg[File]("<dar>")
      .required()
      .action((d, c) => c.copy(darPath = d))

    arg[File]("dev-dar")
      .required()
      .action((d, c) => c.copy(devDarPath = d))

    opt[Unit]('w', "wall-clock-time")
      .action { (_, c) =>
        c.copy(wallclockTime = true)
      }
      .text("Use wall clock time (UTC). When not provided, static time is used.")

    opt[File]("cacrt")
      .optional()
      .action((d, c) => c.copy(rootCa = Some(d)))
  }

  def main(args: Array[String]): Unit = {
    configParser.parse(args, Config(0, null, null, false, None)) match {
      case None =>
        sys.exit(1)
      case Some(config) =>
        val encodedDar: Dar[(PackageId, DamlLf.ArchivePayload)] =
          DarReader().readArchiveFromFile(config.darPath).get
        val dar: Dar[(PackageId, Package)] = encodedDar.map {
          case (pkgId, pkgArchive) => Decode.readArchivePayload(pkgId, pkgArchive)
        }

        println(config.devDarPath)
        val encodedDevDar: Dar[(PackageId, DamlLf.ArchivePayload)] =
          DarReader().readArchiveFromFile(config.devDarPath).get
        val devDar: Dar[(PackageId, Package)] = encodedDevDar.map {
          case (pkgId, pkgArchive) => Decode.readArchivePayload(pkgId, pkgArchive)
        }

        val participantParams =
          Participants(
            Some(ApiParameters("localhost", config.ledgerPort, None, None)),
            Map.empty,
            Map.empty)

        val runner =
          new TestRunner(participantParams, dar, config.wallclockTime, config.rootCa)
        val devRunner =
          new TestRunner(participantParams, devDar, config.wallclockTime, config.rootCa)
        TestQueryContractId(dar, runner).runTests()
        TestQueryContractKey(dar, runner).runTests()
        TraceOrder(dar, runner).runTests()
        Test0(dar, runner).runTests()
        Test1(dar, runner).runTests()
        Test2(dar, runner).runTests()
        Test3(dar, runner).runTests()
        Test4(dar, runner).runTests()
        TestKey(dar, runner).runTests()
        TestCreateAndExercise(dar, runner).runTests()
        GetTime(dar, runner).runTests()
        Sleep(dar, runner).runTests()
        PartyIdHintTest(dar, runner).runTests()
        ListKnownParties(dar, runner).runTests()
        TestStack(dar, runner).runTests()
        TestMaxInboundMessageSize(dar, runner).runTests()
        ScriptExample(dar, runner).runTests()
        TestContractId(devDar, devRunner).runTests()
        // Keep this at the end since it changes the time and we cannot go backwards.
        if (!config.wallclockTime) {
          SetTime(dar, runner).runTests()
        }
    }
  }
}
