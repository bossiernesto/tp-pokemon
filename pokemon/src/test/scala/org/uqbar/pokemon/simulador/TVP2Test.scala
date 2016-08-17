package org.uqbar.pokemon.simulador

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.uqbar.pokemon.simulador.TPV2._
import org.scalatest.BeforeAndAfter

import org.scalatest.Matchers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

class SimuladorV2Test extends FreeSpec with Matchers with BeforeAndAfter {

  val impactrueno = Ataque(Electrico, 30, None)
  val volar = Ataque(Volador, 50, None)
  val clasePikachu = Especie((Electrico, None), None)(200, 80, Incrementos(10, 1, 2, 3))
  val claseGengar = Especie((Fantasma, None), None)(200, 80, Incrementos(15, 13, 12, 1))

  "Centro Pokemon " - {
    
    "Efectos sobre el pokemon" - {
      "Pikachu gana experiencia" in {
        val pikachu = Pokemon(clasePikachu, Macho, Caracteristicas(1, 50, 70, 20, 10,5))
        
        val nuevoPikachu = pikachu.hacer(levantarPesas(10))
        nuevoPikachu.get.caracteristicas.experiencia should be(11)
        
      }
      
      "Gengar no puede levantar pesas" in {
        val gengar = Pokemon(claseGengar, Macho, Caracteristicas(100, 60, 22, 16, 7, 2)) 
        
        a[SimuladorPokemonException] should be thrownBy {
          gengar.hacer(levantarPesas(10))
        }
      }
    }
    
    
    
    "entrenamiento Simple" - {
      "Entrenar a pokachu" in {
        
      }
    }
  }
  
}