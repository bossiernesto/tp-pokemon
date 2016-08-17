package org.uqbar.pokemon.simulador

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.uqbar.pokemon.simulador.SimuladorPokemon._

import org.scalatest.Matchers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

class SumladorV1Test extends PokemonMatchers {

  "Simulador de entrenamiento pokemon" - {

    "Entrenamiento" - {
      "pikachu ataca" in {
        val pikachu = new Pokemon(TipoElectrico, 100, 100, List(), 0, 1, 10)
        val pikachuDespuesAtaque = slam(pikachu)()
        pikachuDespuesAtaque.energia should be(90)
      }

      "Entrenamiento Simple" in {
        val pikachu = new Pokemon(TipoElectrico, 100, 100, List(slam, thundershock), 0, 1, 10)
        val entrenamiento = new EntrenamientoNormal
        val pikachuCansado = entrenamiento(pikachu)()

        pikachu.energia should be(100)
        pikachu.mp should be(100)
        pikachuCansado.energia should be(82)
        pikachuCansado.mp should be(92)

      }

      "Entrenamiento vago" in {
        val charmander = new Pokemon(TipoFuego, 80, 75, List(fireball, growl), 0, 1, 12)
        val entrenamiento = new EntrenamientoLigero
        val charmanderTranca = entrenamiento(charmander)()

        charmanderTranca.energia should be(75)
        charmanderTranca.experiencia should be(7)
        charmanderTranca.mp should be(65)
      }

      "Entrenamiento con bonificacion de Energia" in {
        val charmander = new Pokemon(TipoFuego, 80, 75, List(fireball, growl), 0, 1, 12)
        val entrenamiento = new EntrenamientoLigero
        val bonificacion = Some(new BonificacionExperiencia)
        val charmanderTranca = entrenamiento(charmander)(bonificacion)

        charmanderTranca.energia should be(75)
        charmanderTranca.experiencia should be(14)
        charmanderTranca.mp should be(65)
      }

      "Entrenamiento con bonificacion de Experiencia" in {
        val charmander = new Pokemon(TipoFuego, 80, 75, List(fireball, growl), 0, 1, 12)
        val entrenamiento = new EntrenamientoLigero
        val bonificacion = Some(new BonificacionEnergia)
        val charmanderTranca = entrenamiento(charmander)(bonificacion)

        charmanderTranca.energia should be(77)
        charmanderTranca.experiencia should be(7)
        charmanderTranca.mp should be(65)
      }

    }

    "Dieta diaria" - {
      "pikachu come" in {
        val bonificacion = new BonificacionExperiencia
        val pikachu = new Pokemon(TipoElectrico, 100, 100, List(slam, thundershock), 0, 1, 10)
        val dieta = new DietaCompleta(bonificacion)
        dieta(pikachu).pokemonComido.energia should be(135)
      }

      "entrenador no quiere un pikachu energetico" in {
        val bonificacion = new BonificacionExperiencia
        val pikachu = new Pokemon(TipoElectrico, 100, 100, List(slam, thundershock), 0, 1, 10)
        val dieta = new DietaCompleta(bonificacion)
        val objetivo = ObjetivoEnergia(menorA, 130)

        objetivo.apply(pikachu) should be(true)

        val ResultadoComida(pikachuEnergetico, _) = dieta(pikachu)
        pikachuEnergetico.energia should be(135)
        objetivo.apply(pikachuEnergetico) should be(false)
      }

      "Dieta con bonificacion de entrenamiento" in {
        val bonificacion = new BonificacionExperiencia
        val pikachu = new Pokemon(TipoElectrico, 100, 100, List(slam, thundershock), 0, 1, 10)
        val dieta = new DietaCompleta(bonificacion)

        val ResultadoComida(pikachuSatisfecho, _) = dieta(pikachu)

        pikachuSatisfecho.energia should be(135)
        pikachuSatisfecho.experiencia should be(0)
        pikachuSatisfecho.mp should be(135)
      }
    }

    "Centro de entrenamiento" - {

      "pikachu cansado" in {
        val pikachu = new Pokemon(TipoElectrico, 15, 100, List(slam, slam, thundershock), 0, 1, 10)
        val dieta = new DietaLigera
        val entrenamiento = new EntrenamientoNormal

        val centro = new Simulador
        a[SimuladorPokemonException] should be thrownBy {
          centro.entrenamientoDiario(pikachu, dieta, entrenamiento)
        }

      }

      "entrenar a pikachu" in {
        val pikachu = new Pokemon(TipoElectrico, 15, 100, List(slam, thundershock), 0, 1, 10)
        val dieta = new DietaBalanceada
        val entrenamiento = new EntrenamientoNormal
        val objectivos = List(ObjetivoExperiencia(mayorIgualA, 100))

        val centro = new Simulador
        val resultado: ResultadoSimulacion = centro.entrenarPokemon(pikachu, objectivos, entrenamiento, dieta)

        val expected = pikachu.copy(experiencia = 102)
        resultado should generatePokemon(expected)
        resultado should not(generatePokemon(pikachu))
      }

    }
  }

}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// Pokemon Matchers
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

abstract class PokemonMatchers extends FreeSpec with Matchers {

  class PokemonResultMatches(pokemon: Pokemon) extends Matcher[ResultadoSimulacion] {
    def apply(left: ResultadoSimulacion) = {
      val result = left match {
        case SimulacionExitosa(pokemonResultante, dias) => {
          pokemonResultante.experiencia == pokemon.experiencia
        }
        case _ => false
      }
      MatchResult(result, s"""Resultado $left did not generated a pokemon "$pokemon"""", s"""Resultado $left did generated a pokemon "$pokemon"""")
    }
  }

  def generatePokemon(expected: Pokemon) = new PokemonResultMatches(expected)
}
