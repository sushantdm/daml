// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.engine.script.test

import com.daml.ledger.api.testing.utils.SuiteResourceManagementAroundAll
import com.daml.lf.data.{FrontStack, FrontStackCons, Numeric}
import com.daml.lf.data.Ref._
import com.daml.lf.speedy.SValue
import com.daml.lf.speedy.SValue._
import org.scalatest._
import spray.json.{JsNumber, JsObject, JsString}

abstract class AbstractFuncIT
    extends AsyncWordSpec
    with SandboxParticipantFixture
    with Matchers
    with SuiteResourceManagementAroundAll {
  val (stableDar, stableEnvIface) = readDar(stableDarFile)

  s"DAML Script func tests: ${timeMode}" can {
    "test0" should {
      "create two accepted proposals" in {
        for {
          clients <- participantClients()
          SRecord(_, _, vals) <- run(
            clients,
            QualifiedName.assertFromString("ScriptTest:test0"),
            dar = stableDar)
        } yield {
          assert(vals.size == 5)
          val alice = vals.get(0) match {
            case SParty(alice) => alice
            case v => fail(s"Expected SParty but got $v")
          }
          val bob = vals.get(1) match {
            case SParty(bob) => bob
            case v => fail(s"Expected SParty but got $v")
          }
          // allocateParty should return a fresh party
          assert (alice != bob)
          vals.get(2) match {
            case SList(FrontStackCons(SRecord(_, _, t1), FrontStackCons(SRecord(_, _, t2), FrontStack()))) =>
              t1 should contain theSameElementsInOrderAs(Seq(SParty(alice), SParty(bob)))
              t2 should contain theSameElementsInOrderAs(Seq(SParty(alice), SParty(bob)))
            case v => fail(s"Expected SList but got $v")
          }
          assert(vals.get(3) == SList(FrontStack.empty))
          vals.get(4) match {
            case SList(FrontStackCons(SRecord(_, _, vals), FrontStack())) =>
              vals should contain theSameElementsInOrderAs(Seq[SValue](SParty(alice), SInt64(42)))
            case v => fail(s"Expected a single SRecord but got $v")
          }
        }
      }
    }
    "test1" should {
      "handle numerics correctly" in {
        for {
          clients <- participantClients()
          v <- run(
            clients,
            QualifiedName.assertFromString("ScriptTest:test1"),
            dar = stableDar)
        } yield {
          assert(v == SNumeric(Numeric.assertFromString("2.12000000000")))
        }
      }
    }
    "test2" should {
      "extract value from input" in {
        for {
          clients <- participantClients()
          v <- run(clients, QualifiedName.assertFromString("ScriptTest:test2"), dar = stableDar, inputValue = Some(JsObject(("p", JsString("Alice")), ("v", JsNumber(42)))))
        } yield {
          assert(v == SInt64(42))
        }
      }
    }
    "test3" should {
      "support submitMustFail" in {
        for {
          clients <- participantClients()
          v <- run(clients, QualifiedName.assertFromString("ScriptTest:test3"), dar = stableDar)
        } yield {
          assert (v == SUnit)
        }
      }
    }
    "test4" should {
      "return new contract in query" in {
        for {
          clients <- participantClients()
          SRecord(_, _, vals) <- run(clients, QualifiedName.assertFromString("ScriptTest:test4"), dar = stableDar)
        } yield {
          assert (vals.size == 2)
          assert(vals.get(0) == vals.get(1))
        }
      }
    }
    "testKey" should {
      "support exerciseByKeyCmd" in {
        for {
          clients <- participantClients()
          SRecord(_, _, vals) <- run(clients, QualifiedName.assertFromString("ScriptTest:testKey"), dar = stableDar)
        } yield {
          assert(vals.size == 2)
          assert(vals.get(0) == vals.get(1))
        }
      }
    }
    "testCreateAndExercise" should {
      "support createAndExerciseCmd" in {
        for {
          clients <- participantClients()
          v <- run(clients, QualifiedName.assertFromString("ScriptTest:testCreateAndExercise"), dar = stableDar)
        } yield {
          assert(v == SInt64(42))
        }
      }
    }
    "testGetTime" should {
      "not go backwards in time" in {
    for {
      clients <- participantClients()
      SRecord(_, _, vals) <- run(clients, QualifiedName.assertFromString("ScriptTest:testGetTime"), dar = stableDar)
    } yield {
      assert(vals.size == 2)
      val t0 = vals.get(0) match {
        case STimestamp(t0) => t0
        case v => fail(s"Expected STimestamp but got $v")
      }
      val t1 = vals.get(1) match {
        case STimestamp(t1) => t1
        case v => fail(s"Expected STimestamp but got $v")
      }
      // Note that even in wallclock mode we cannot use strict inequality due to time
      // resolution (observed in CI)
      assert(t0 <= t1)
    }


      }
    }
  }
}
