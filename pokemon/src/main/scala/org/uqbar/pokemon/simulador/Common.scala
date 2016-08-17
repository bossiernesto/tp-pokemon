package org.uqbar.pokemon.simulador
import scala.language.implicitConversions
import scala.language.reflectiveCalls

case class Bool(b: Boolean) {
  def ?[X](t: => X) = new {
    def |(f: => X) = if (b) t else f
  }
}

object Bool {
  implicit def BooleanBool(b: Boolean) = Bool(b)
}

case class SimuladorPokemonException(message: String) extends RuntimeException(message)