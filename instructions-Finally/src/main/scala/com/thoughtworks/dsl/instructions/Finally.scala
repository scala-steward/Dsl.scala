package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl.Instruction

final case class Finally[Domain](hook: Domain => Domain) extends AnyVal with Instruction[Finally[Domain], Unit]
