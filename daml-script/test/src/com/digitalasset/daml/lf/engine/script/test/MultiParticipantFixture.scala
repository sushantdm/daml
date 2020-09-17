// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.engine.script.test

import java.nio.file.Paths

import com.daml.bazeltools.BazelRunfiles._
import com.daml.ledger.api.testing.utils.{AkkaBeforeAndAfterAll, OwnedResource, SuiteResource}
import com.daml.ledger.api.tls.TlsConfiguration
import com.daml.ledger.on.memory.{ExtraConfig, Owner}
import com.daml.ledger.participant.state.kvutils.app.ParticipantConfig
import com.daml.ledger.participant.state.kvutils.{app => kvutils}
import com.daml.ledger.participant.state.v1
import com.daml.lf.engine.script._
import com.daml.ports.Port
import org.scalatest.Suite

import scala.concurrent.ExecutionContext

trait MultiParticipantFixture
    extends AbstractScriptTest
    with SuiteResource[(Port, Port)]
    with AkkaBeforeAndAfterAll {
  self: Suite =>
  private def darFile = Paths.get(rlocation("daml-script/test/script-test.dar"))

  private val participantId1 = v1.ParticipantId.assertFromString("participant1")
  private val participant1 = ParticipantConfig(
    participantId = participantId1,
    address = Some("localhost"),
    port = Port(6865),
    portFile = None,
    serverJdbcUrl = ParticipantConfig.defaultIndexJdbcUrl(participantId1),
    allowExistingSchemaForIndex = false,
    maxCommandsInFlight = None
  )
  private val participantId2 = v1.ParticipantId.assertFromString("participant2")
  private val participant2 = ParticipantConfig(
    participantId = participantId2,
    address = Some("localhost"),
    port = Port(6866),
    portFile = None,
    serverJdbcUrl = ParticipantConfig.defaultIndexJdbcUrl(participantId2),
    allowExistingSchemaForIndex = false,
    maxCommandsInFlight = None
  )
  override protected lazy val suiteResource = {
    implicit val ec: ExecutionContext = system.dispatcher
    new OwnedResource[(Port, Port)](
      for {
        _ <- Owner(
          kvutils.Config
            .createDefault(ExtraConfig.reasonableDefault)
            .copy(
              participants = Seq(
                participant1,
                participant2
              ),
              archiveFiles = Seq(
                darFile
              )))
      } yield (participant1.port, participant2.port)
    )
  }

  def participantClients() = {
    implicit val ec: ExecutionContext = system.dispatcher
    val params = Participants(
      None,
      Seq(
        (Participant("one"), ApiParameters("localhost", suiteResource.value._1.value, None, None)),
        (Participant("two"), ApiParameters("localhost", suiteResource.value._2.value, None, None))
      ).toMap,
      Map.empty
    )
    Runner.connect(
      params,
      tlsConfig = TlsConfiguration(false, None, None, None),
      maxInboundMessageSize = RunnerConfig.DefaultMaxInboundMessageSize)
  }

  override def timeMode: ScriptTimeMode = ScriptTimeMode.WallClock
}
