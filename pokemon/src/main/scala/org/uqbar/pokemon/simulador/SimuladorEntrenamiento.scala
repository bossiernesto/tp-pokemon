package org.uqbar.pokemon.simulador

import scala.language.implicitConversions
import java.lang.reflect.Type
import Bool._
import scala.language.reflectiveCalls

package object SimuladorPokemon {

  implicit def double2int(d: Double): Int = d.toInt

  /*Estado de Pokemon*/
  case class Pokemon(tipo: TipoPokemon, energia: Int, mp: Int, ataques: List[Ataque], experiencia: Int, nivelEvolucion: Int, peso: Int) {}

  trait TipoPokemon
  object TipoFuego extends TipoPokemon
  object TipoAgua extends TipoPokemon
  object TipoElectrico extends TipoPokemon
  object TipoVolador extends TipoPokemon

  /*Tuple Comparator structure*/
  type TupleComp = (Int, Int) => Boolean
  type TupleAtaque = (List[Ataque], Ataque) => Boolean
  type TupleTipoPokemon = (TipoPokemon, TipoPokemon) => Boolean

  /*comparators*/
  val mayorA: TupleComp = _ > _
  val mayorIgualA: TupleComp = _ >= _
  val menorA: TupleComp = _ < _
  val menorIgualA: TupleComp = _ <= _
  val igualA: TupleComp = _ == _
  val contiene: TupleAtaque = _ contains _
  val esDelTipo: TupleTipoPokemon = _ == _

  /*Objetivos*/
  def nivelEvolucionObjetivo(tupl: TupleComp)(n: Int)(pokemon: Pokemon) = tupl(pokemon.nivelEvolucion, n)
  def energiaObjetivo(tupl: TupleComp)(e: Int)(pokemon: Pokemon) = tupl(pokemon.energia, e)
  def pesoObjectivo(tupl: TupleComp)(p: Int)(pokemon: Pokemon) = tupl(pokemon.peso, p)
  def AtaqueIncludedObjetivo(tupl: TupleAtaque)(ataque: Ataque)(pokemon: Pokemon) = tupl(pokemon.ataques, ataque)
  def experienciaObjetivo(tupl: TupleComp)(e: Int)(pokemon: Pokemon) = tupl(pokemon.experiencia, e)
  def tipoPokemonObjetivo(tupl: TupleTipoPokemon)(tipo: TipoPokemon)(pokemon: Pokemon) = tupl(pokemon.tipo, tipo)

  def objetivoEvolucion: TupleComp => Int => Pokemon => Boolean = nivelEvolucionObjetivo _
  def objectivoEnergia: TupleComp => Int => Pokemon => Boolean = energiaObjetivo _
  def objetivoPeso: TupleComp => Int => Pokemon => Boolean = pesoObjectivo _
  def objetivoAtaque: TupleAtaque => Ataque => Pokemon => Boolean = AtaqueIncludedObjetivo _
  def objectivoExperiencia: TupleComp => Int => Pokemon => Boolean = experienciaObjetivo _
  def objetivoTipoPokemon: TupleTipoPokemon => TipoPokemon => Pokemon => Boolean = tipoPokemonObjetivo _

  object Objetivo
  sealed trait Objetivo {
    def apply: Pokemon => Boolean
  }

  object ObjetivoPeso
  case class ObjetivoPeso(condicion: TupleComp, peso: Int) extends Objetivo {
    def apply: Pokemon => Boolean = objetivoPeso(condicion)(peso)
  }

  object ObjetivoEnergia
  case class ObjetivoEnergia(condicion: TupleComp, energiaObjetivo: Int) extends Objetivo {
    def apply: Pokemon => Boolean = objectivoEnergia(condicion)(energiaObjetivo)
  }

  object ObjetivoAtaques
  case class ObjetivoAtaques(condicion: TupleAtaque, ataque: Ataque) extends Objetivo {
    def apply: Pokemon => Boolean = objetivoAtaque(condicion)(ataque)
  }

  object ObjetivoEvolucion
  case class ObjetivoEvolucion(condicion: TupleComp, nivelEvolucion: Int) extends Objetivo {
    def apply: Pokemon => Boolean = objetivoEvolucion(condicion)(nivelEvolucion)
  }

  object ObjetivoExperiencia
  case class ObjetivoExperiencia(condicion: TupleComp, nivelExperiencia: Int) extends Objetivo {
    def apply: Pokemon => Boolean = objectivoExperiencia(condicion)(nivelExperiencia)
  }

  object ObjetivoTipoPokemon
  case class ObjetivoTipoPokemon(condicion: TupleTipoPokemon, tipo: TipoPokemon) extends Objetivo {
    def apply: Pokemon => Boolean = objetivoTipoPokemon(condicion)(tipo)
  }

  /*Ataques*/
  object Ataque
  case class Ataque(gastomp: Int, gastoenergia: Int, experienciaGanada: Int) {
    def apply(pokemon: Pokemon)(bonificacion: Option[Bonificacion] = None): Pokemon = {
      val nuevoMP = pokemon.mp - gastomp
      if (nuevoMP > 0) {
        val aumentoExperiencia = pokemon.experiencia + experienciaGanada
        val disminucionEnergia = pokemon.energia - gastoenergia

        return bonificacion getOrElse (None) match {
          case b: BonificacionExperiencia => pokemon.copy(mp = nuevoMP, energia = disminucionEnergia, experiencia = pokemon.experiencia + experienciaGanada * b.bonus)
          case b: BonificacionEnergia     => pokemon.copy(mp = nuevoMP, energia = pokemon.energia - gastoenergia * b.bonus, experiencia = aumentoExperiencia)
          case _                          => pokemon.copy(mp = nuevoMP, energia = disminucionEnergia, experiencia = aumentoExperiencia)
        }
      }
      pokemon
    }
  }

  val fly = Ataque(1, 2, 2)
  val cut = Ataque(13, 10, 5)
  val slam = Ataque(5, 10, 9)
  val doubleTeam = Ataque(7, 12, 6)
  val thundershock = Ataque(3, 8, 8)
  val growl = Ataque(6, 9, 12)
  val fireball = Ataque(10, 5, 7)
  val flamethrower = Ataque(20, 12, 9)
  val waterGun = Ataque(14, 13, 9)

  /*Dieta*/
  sealed trait ResultadoDieta
  case class ResultadoComida(pokemonComido: Pokemon, bonificacion: Option[Bonificacion] = None) extends ResultadoDieta

  object Dieta
  sealed trait Dieta {
    val aumentoEnergia: Int
    val aumentoMp: Int
    def apply(pokemon: Pokemon): ResultadoComida = {
      val energiaGanada = pokemon.energia + aumentoEnergia
      val mpGanado = pokemon.mp + aumentoMp
      ResultadoComida(pokemon.copy(energia = energiaGanada, mp = mpGanado), None)
    }
  }

  class DietaBalanceada extends Dieta {
    val aumentoEnergia = 22
    val aumentoMp = 20
  }

  class DietaLigera extends Dieta {
    val aumentoEnergia = 12
    val aumentoMp = 15
  }

  class DietaCompleta(bonificacion: Bonificacion) extends Dieta {
    val aumentoEnergia = 35
    val aumentoMp = 35
    override def apply(pokemon: Pokemon): ResultadoComida = {
      val resultado = super.apply(pokemon)
      ResultadoComida(resultado.pokemonComido, Some(bonificacion))
    }
  }

  /*Bonificaciones*/
  object Bonificacion
  class Bonificacion(bonus: Double = 1)
  case class BonificacionEnergia(bonus: Double = 0.5) extends Bonificacion
  case class BonificacionExperiencia(bonus: Double = 2) extends Bonificacion

  /*Entrenamiento*/
  object Entrenamiento
  sealed trait Entrenamiento {
    def apply(pokemon: Pokemon)(bonificacion: Option[Bonificacion] = None): Pokemon
  }

  class EntrenamientoLigero extends Entrenamiento {
    def apply(pokemon: Pokemon)(bonificacion: Option[Bonificacion] = None): Pokemon = {
      pokemon.ataques.head.apply(pokemon)(bonificacion)
    }
  }

  class EntrenamientoNormal extends Entrenamiento {
    def apply(pokemon: Pokemon)(bonificacion: Option[Bonificacion] = None): Pokemon = {
      pokemon.ataques.foldLeft(pokemon) {
        case (pokemon, ataque) => ataque.apply(pokemon)(bonificacion)
      }
    }
  }

  case class Actividad(gastoEnergia: Int, experienciaGanada: Int) {
    def apply(pokemon: Pokemon)(bonificacion: Option[Bonificacion]): Pokemon = {
      val perdidaEnergia = pokemon.energia - gastoEnergia
      val experienciaGanadaTotal = pokemon.energia + experienciaGanada

      bonificacion.getOrElse(None) match {
        case b: BonificacionExperiencia => pokemon.copy(energia = perdidaEnergia, experiencia = pokemon.energia + experienciaGanada * b.bonus)
        case _                          => pokemon.copy(energia = perdidaEnergia, experiencia = experienciaGanadaTotal)
      }
    }
  }

  val saltarAros = Actividad(10, 5)
  val nadar = Actividad(5, 10)
  val trepar = Actividad(8, 9)

  class EntrenamientoActividades(actividades: List[Actividad] = List(saltarAros, nadar, trepar)) extends Entrenamiento {
    def apply(pokemon: Pokemon)(bonificacion: Option[Bonificacion] = None): Pokemon = {
      actividades.foldLeft(pokemon) {
        case (pokemon, actividad) => actividad.apply(pokemon)(bonificacion)
      }
    }
  }

  object Pokedex {

    object AtaqueAprendizaje
    case class AtaqueAprendizaje(ataque: Ataque, condiciones: List[Objetivo])

    val pokedexAtaques: List[AtaqueAprendizaje] = List(AtaqueAprendizaje(thundershock,
      List(ObjetivoExperiencia(mayorIgualA, 200), ObjetivoTipoPokemon(esDelTipo, TipoElectrico))),
      AtaqueAprendizaje(fly, List(ObjetivoTipoPokemon(esDelTipo, TipoVolador), ObjetivoExperiencia(mayorIgualA, 150))))
  }

  /*Aprendizaje de ataques y evolucion*/
  trait Aprendizaje
  object Aprendizaje {

    def apply(pokemon: Pokemon): Pokemon = chequearAtaqueNuevo(pokemon)

    def chequearAtaqueNuevo(pokemon: Pokemon): Pokemon = {

      Pokedex.pokedexAtaques.filter {
        ataqueAprendizaje => ataqueAprendizaje.condiciones.forall(c => c.apply(pokemon))
      }.foldLeft(pokemon) {
        case (pokemon, ataqueAprendido) => {
          (pokemon.ataques contains ataqueAprendido.ataque) ? pokemon.copy(ataques = pokemon.ataques :+ ataqueAprendido.ataque) | pokemon
        }
      }
    }
  }

  /*Resultado Entrenamiento*/
  sealed trait ResultadoSimulacion
  case class SimulacionExitosa(pokemon: Pokemon, diasEntrenamiento: Int) extends ResultadoSimulacion
  case class SimulacionFallida(pokemon: Pokemon, diasEntrenamiento: Int, razon: Exception) extends ResultadoSimulacion

  class Simulador {

    val energiaMinima = ObjetivoEnergia(mayorA, 0)
    val pesoMinimo = ObjetivoPeso(mayorIgualA, 5)
    val condicionesMinimas = List(energiaMinima, pesoMinimo)

    def entrenarPokemon(pokemon: Pokemon, objectivos: List[Objetivo], entrenamiento: Entrenamiento, dieta: Dieta): ResultadoSimulacion = {
      var dias: Int = 0
      try {
        objectivos.forall(c => c.apply(pokemon)) match {
          case true => SimulacionExitosa(pokemon, dias)
          case false => {
            val pokemonEntrenado = this.entrenamientoDiario(pokemon, dieta, entrenamiento)
            dias = aumentarDia(dias)
            this.entrenarPokemon(pokemonEntrenado.get, objectivos, entrenamiento, dieta)
          }
        }
      } catch {
        case e: SimuladorPokemonException => SimulacionFallida(pokemon, dias, e)
      }
    }

    def aumentarDia(dias: Int): Int = dias + 1

    def estaEnCondiciones(pokemon: Pokemon): Option[Pokemon] = {
      condicionesMinimas.forall(c => c.apply(pokemon)) match {
        case true  => Some(pokemon)
        case false => throw SimuladorPokemonException("Pokemon en Peligro")
      }
    }

    def entrenamientoDiario(pokemon: Pokemon, dieta: Dieta, entrenamiento: Entrenamiento): Option[Pokemon] = {
      val ResultadoComida(pokemonComido, bonificacion) = dieta(pokemon)
      this.estaEnCondiciones(Aprendizaje(entrenamiento(pokemonComido)(bonificacion)))
    }

  }

}