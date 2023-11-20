package solucao

import java.time.LocalDateTime
import scala.annotation.tailrec


case class Intervalo(inicio: LocalDateTime, fim: LocalDateTime) {
  def contem(data: LocalDateTime): Boolean = data.equals(inicio) || data.equals(fim) || data.isAfter(inicio) && data.isBefore(fim)

  def naoContem(data: LocalDateTime): Boolean = !contem(data)

  def estaAntesDaData(data: LocalDateTime): Boolean = fim.isBefore(data)

  def naoIntersecta(outro: Intervalo): Boolean = !intersecta(outro)

  private def intersecta(outro: Intervalo) = {
    val esseAntesDoOutro = fim.isBefore(outro.inicio)
    val esseDepoisDoOutro = inicio.isAfter(outro.fim)
    !(esseAntesDoOutro || esseDepoisDoOutro)
  }
}


object Solucao {
  def filtrarDatasDentroDeIntervalosMLogMMaisN(datas: Seq[LocalDateTime], intervalos: Seq[Intervalo]): Seq[Data] =
    datas.sorted.foldLeft((Seq.empty[LocalDateTime], intervalos.sortBy(_.inicio))) { case ((acc, intervalos), data) =>
      val todosIntervalosQuePodemConter = intervalos.dropWhile(_.estaAntesDaData(data))
      val ultimaPosicaoQueContem = todosIntervalosQuePodemConter.lastIndexWhere(_.contem(data))
      val intervalosComOUltimoQuePodeConter = todosIntervalosQuePodemConter.drop(ultimaPosicaoQueContem)
      val accAtualizado = if (intervalosComOUltimoQuePodeConter.headOption.map(_.naoContem(data)).getOrElse(true)) acc :+ data else acc
      (accAtualizado, intervalosComOUltimoQuePodeConter)
    }._1

  def filtrarDatasDentroDeIntervalosMLogN(datas: Seq[Data], intervalos: Seq[Intervalo]): Seq[Data] = {
    def dataMaxima(a: Data, b: Data) = if (a.isAfter(b)) a else b

    def algumIntervaloContemData(data: Data, inicios: Seq[Data], finais: Seq[Data]) = {
      @tailrec
      def f(i: Int, j: Int, maximoAnteriorIdx: Option[Int]): Option[Int] = {
        if (i > j) {
          maximoAnteriorIdx
        } else {
          val k = i + (j - i) / 2
          if (inicios(k).isAfter(data)) {
            f(i, k - 1, maximoAnteriorIdx)
          } else {
            f(k + 1, j, Some(k))
          }
        }
      }

      f(0, inicios.length - 1, None).exists(maximoAnteriorIdx => finais(maximoAnteriorIdx).equals(data) || finais(maximoAnteriorIdx).isAfter(data))
    }

    val intervalosDisjuntos = intervalos.sortBy(_.inicio).foldLeft(Seq.empty[Intervalo]) { case (acc, intervalo) =>
      val intervaloPrevio = acc.lastOption.getOrElse(Intervalo(LocalDateTime.MIN, LocalDateTime.MIN))
      if (intervalo.naoIntersecta(intervaloPrevio)) {
        acc :+ intervalo
      } else {
        acc.init :+ Intervalo(intervaloPrevio.inicio, dataMaxima(intervaloPrevio.fim, intervalo.fim))
      }
    }

    val inicios = intervalosDisjuntos.map(_.inicio)
    val finais = intervalosDisjuntos.map(_.fim)
    datas.filterNot(algumIntervaloContemData(_, inicios, finais))
  }
}
