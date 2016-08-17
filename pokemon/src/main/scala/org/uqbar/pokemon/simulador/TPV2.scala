package org.uqbar.pokemon.simulador

import scala.util.Try

object TPV2 {

	case class Pokemon(
		especie: Especie,
		genero: Genero,
		caracteristicas: Caracteristicas,
		ataques: Map[Ataque, (Int, Int)] = Map(),
		estado: Estado = Sano
	) {

		require(experiencia > 0, "La Experiencia debe ser positiva")
		require(energiaMaxima > 0, "La Energia Maxima debe ser positiva")
		require(energia > 0 && energia < energiaMaxima, s"La Energia debe ir de 0 a $energiaMaxima")
		require(peso > 0, "El Peso debe ser positivo")
		require(fuerza >= 1 && fuerza <= 100, "La Fuerza debe ir de 1 a 100")
		require(velocidad >= 1 && velocidad <= 100, "La Velocidad debe ir de 1 a 100")

		def energia = caracteristicas.energia
		def experiencia = caracteristicas.experiencia
		def energiaMaxima = caracteristicas.energiaMaxima + nivel * especie.incrementos.energiaMaxima
		def peso = caracteristicas.peso + nivel * especie.incrementos.peso
		def fuerza = caracteristicas.fuerza + nivel * especie.incrementos.fuerza
		def velocidad = caracteristicas.velocidad + nivel * especie.incrementos.velocidad

		def nivel: Int = {
			def nivelR(expNivelAnterior: Int, nivelAnterior: Int): Int = {
				val expNivelSiguiente = 2 * expNivelAnterior + especie.resistenciaEvolutiva
				if (experiencia > expNivelSiguiente) nivelAnterior else nivelR(expNivelSiguiente, nivelAnterior + 1)
			}
			nivelR(0, 1)
		}

		def ganarExp(exp: Int) = {
			val crecido = copy(caracteristicas = caracteristicas.copy(experiencia = experiencia + exp))
			if (crecido.nivel > this.nivel) especie.condicionEvolutiva.map(_.subioDeNivel(crecido)).getOrElse(crecido)
			else crecido
		}

		def energia(delta: Int) = copy(caracteristicas = caracteristicas.copy(energia = energia + delta min energiaMaxima))
		def fuerza(delta: Int) = copy(caracteristicas = caracteristicas.copy(fuerza = fuerza + delta))
		def velocidad(delta: Int) = copy(caracteristicas = caracteristicas.copy(velocidad = velocidad + delta))
		def estado(estado: Estado) = copy(estado = estado)
		
		def hacer(actividad: Actividad): Try[Pokemon] = Try{
			estado match {
				case Dormido(n) => estado(Dormido(n - 1))
				case _ => actividad(this)
			}
		}
	}
	
	case class Caracteristicas(
		experiencia: Int,
		energia: Int,
		energiaMaxima: Int,
		peso: Int,
		fuerza: Int,
		velocidad: Int
	)
	
	sealed trait Genero
	case object Macho extends Genero
	case object Hembra extends Genero

	sealed trait Estado
	case class Dormido(turnosPendientes: Int = 3) extends Estado
	case object Envenenado extends Estado
	case object Paralizado extends Estado
	case object KO extends Estado
	case object Sano extends Estado

	case class Ataque(tipo: Tipo, puntosDeAtaqueMaximo: Int, efecto: Option[Pokemon => Pokemon])
	
	
	//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
	// ESPECIES
	//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄
	
	case class Especie(
		tipos: (Tipo, Option[Tipo]),
		condicionEvolutiva: Option[CondicionEvolutiva] = None)(
		val resistenciaEvolutiva: Int = 200,
		val pesoMaximo: Int = 80,
		val incrementos: Incrementos
	)
	case class Incrementos (
		energiaMaxima: Int = 0,
		peso: Int = 0,
		fuerza: Int = 0,
		velocidad: Int = 0
	)
	
	class CondicionEvolutiva(evolucion: Especie) { def subioDeNivel(pokemon: Pokemon) = pokemon	}
	case class Intercambiar(evolucion: Especie) extends CondicionEvolutiva(evolucion)
	case class UsarPiedra(piedra: Piedra, evolucion: Especie) extends CondicionEvolutiva(evolucion)
	case class Nivel(n: Int, evolucion: Especie) extends CondicionEvolutiva(evolucion) {
		override def subioDeNivel(pokemon: Pokemon) = if (pokemon.nivel >= n) pokemon.copy(especie = evolucion) else pokemon
	}
	
	//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
	// TIPOS
	//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄
	
	trait Tipo {
		def tiposInferiores: List[Tipo]
		def leGanaA(otro: Tipo) = tiposInferiores.contains(otro)
	}
	case object Fuego extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Planta, Hielo, Bicho) }
	case object Agua extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Fuego, Tierra, Roca) }
	case object Planta extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Agua, Tierra, Roca) }
	case object Tierra extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Fuego, Electrico, Veneno, Roca) }
	case object Hielo extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Planta, Tierra, Volador, Dragon) }
	case object Roca extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Fuego, Hielo, Volador, Bicho) }
	case object Electrico extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Agua, Volador) }
	case object Psiquico extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Pelea, Veneno) }
	case object Pelea extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Normal, Hielo, Roca) }
	case object Fantasma extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Psiquico, Fantasma) }
	case object Volador extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Planta, Pelea, Bicho) }
	case object Bicho extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Planta, Psiquico) }
	case object Veneno extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Planta) }
	case object Dragon extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Dragon) }
	case object Normal extends Tipo { lazy val tiposInferiores: List[Tipo] = List() }
	
	
	//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
	// OBJETOS
	//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄
	
	trait Objeto
	trait Piedra extends Objeto
	case class PiedraDeTipo(tipo: Tipo) extends Piedra
	case object PiedraLunar extends Piedra
	case object Pocion extends Objeto
	case object Antidoto extends Objeto
	case object Ether extends Objeto
	case object Hierro extends Objeto
	case object Calcio extends Objeto
	case object Zinc extends Objeto

	//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
	// ACTIVIDADES
	//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

	type Actividad = Pokemon => Pokemon
	
	def realizar(ataque: Ataque)(pokemon: Pokemon) = {
		val (ap, apMax) = pokemon.ataques(ataque)

		require(ap > 0, "El pokémon debe saber el ataque y tener APs suficientes")

		val pokemonCrecido = pokemon.copy(ataques = pokemon.ataques.updated(ataque, (ap - 1, apMax))).ganarExp(
			(ataque.tipo, pokemon.especie.tipos,pokemon.genero) match {
				case (Dragon, _, _) => 80
				case (tipoAtaque, (tipo,_), _) if tipoAtaque == tipo => 50
				case (tipoAtaque, (_, Some(tipo)), Macho) if tipoAtaque == tipo => 20
				case (tipoAtaque, (_,Some(tipo)), Hembra) if tipoAtaque == tipo => 40
			}
		)

		ataque.efecto.fold(pokemonCrecido)(f => f(pokemonCrecido))
	}

	def levantarPesas(kilos: Int)(pokemon: Pokemon) = {
		require(pokemon.especie.tipos._1 != Fantasma && pokemon.especie.tipos._2 != Some(Fantasma), "Los fantasmas no levantan pesas")

		pokemon match {
			case Pokemon(_, _, _, _, Paralizado) => pokemon.estado(KO)
			case _ if pokemon.fuerza * 10 < kilos => pokemon.estado(Paralizado)
			case Pokemon(Especie((Pelea,_), _), _, _, _, _) => pokemon.ganarExp(kilos * 2)
			case Pokemon(Especie((_, Some(Pelea)), _), _, _, _, _) => pokemon.ganarExp(kilos * 2)
			case _ => pokemon.ganarExp(kilos)
		}
	}

	def nadar(minutos: Int)(pokemon: Pokemon) = {
		pokemon.especie.tipos match {
			case (tipo, _) if Agua leGanaA tipo => pokemon.estado(KO)
			case (_, Some(tipo)) if Agua leGanaA tipo => pokemon.estado(KO)
			case (Agua, _) => pokemon.energia(-minutos).velocidad(minutos / 60).ganarExp(minutos * 200)
			case _ => pokemon.energia(-minutos).ganarExp(minutos * 200)
		}
	}

	def aprender(ataque: Ataque)(pokemon: Pokemon) = {
		(ataque.tipo, pokemon.especie.tipos) match {
			case (Normal, _) => pokemon.copy(ataques = pokemon.ataques.updated(ataque, (ataque.puntosDeAtaqueMaximo, ataque.puntosDeAtaqueMaximo)))
			case (tipoAtaque, (tipo, _)) if tipoAtaque == tipo => pokemon.copy(ataques = pokemon.ataques.updated(ataque, (ataque.puntosDeAtaqueMaximo, ataque.puntosDeAtaqueMaximo)))
			case (tipoAtaque, (_, Some(tipo))) if tipoAtaque == tipo => pokemon.copy(ataques = pokemon.ataques.updated(ataque, (ataque.puntosDeAtaqueMaximo, ataque.puntosDeAtaqueMaximo)))
			case _ => pokemon.estado(KO)
		}
	}

	def usar(objeto: Objeto)(pokemon: Pokemon) = {
		(objeto, pokemon.especie) match {
			case (PiedraLunar, Especie(_, Some(UsarPiedra(PiedraLunar, evolucion)))) => pokemon.copy(especie = evolucion)
			case (piedra: Piedra, Especie(_, Some(UsarPiedra(piedraRequerida, evolucion)))) if piedra == piedraRequerida => pokemon.copy(especie = evolucion)
			case (PiedraDeTipo(tipoPiedra), Especie((tipo, _), _)) if tipoPiedra leGanaA tipo => pokemon.estado(Envenenado)
			case (Pocion, _) => pokemon.energia(50)
			case (Antidoto, _) if pokemon.estado == Envenenado => pokemon.copy(estado = Sano)
			case (Ether, _) if pokemon.estado != KO => pokemon.estado(Sano)
			case (Hierro, _) => pokemon.fuerza(5)
			case (Calcio, _) => pokemon.velocidad(5)
			case (Zinc, _) => pokemon.copy(ataques = pokemon.ataques.mapValues{ case (ap, maxAP) => (ap, maxAP + 2) })
			case _ => pokemon
		}
	}

	def descansar(pokemon: Pokemon) = {
		val descansado = pokemon.copy(ataques = pokemon.ataques.mapValues{ case (_, maxAP) => (maxAP, maxAP) })
		if (descansado.estado == Sano && descansado.energia < descansado.energiaMaxima / 2) descansado.estado(Dormido()) else descansado
	}

	def fingirIntercambio(pokemon: Pokemon) = {
		pokemon.especie.condicionEvolutiva match {
			case Some(Intercambiar(evolucion)) => pokemon.copy(especie = evolucion)
			case _ => pokemon
		}
	}

	
	//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
	// RUTINAS
	//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄
	
	class Rutina(val nombre: String)(actividades: Actividad*) {
		def apply(pokemon: Pokemon) = Try((pokemon /: actividades){ (p, a) => a(p) })
	}

	def mejorRutina(criterio: Pokemon => Int)(pokemon: Pokemon)(rutinas: Rutina*) = {
		val resultados = for {
			rutina <- rutinas
			pokemonEntrenado <- rutina(pokemon).toOption
			valor = criterio(pokemonEntrenado)
		} yield (rutina.nombre, valor)

		resultados.sortBy(_._1).reverse.headOption
	}

	//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
	// EJEMPLO DE USO
	//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

	val impactrueno = Ataque(Electrico, 30, None)
	val ejemploDeRutina = new Rutina("Iron Man")(nadar(30), usar(Hierro), usar(Hierro), descansar, realizar(impactrueno))
}