// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.engine.script.test

import com.daml.lf.engine.script.ScriptTimeMode

final class FuncWallClockIT
    extends AbstractFuncIT {
  override def timeMode = ScriptTimeMode.WallClock
}
