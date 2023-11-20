import org.scalatest.funspec.AnyFunSpec
import solucao.{Intervalo, Solucao, Data}

import java.time.LocalDateTime


class Solucao extends AnyFunSpec {
  type TempoExecucaoMs = Long

  val Data: (Int, Int, Int) => Data = LocalDateTime.of(_: Int, _: Int, _: Int, 0, 0, 0, 0)

  def medirTempo[T](bloco: => T): (T, TempoExecucaoMs) = {
    val antes = System.currentTimeMillis
    val resultado = bloco
    (resultado, System.currentTimeMillis - antes)
  }

  def testarFiltragemDeDatas(f: (Seq[Data], Seq[Intervalo]) => Seq[Data], descricao: String): Unit = {
    def testar(datas: Seq[Data], intervalos: Seq[Intervalo], datasEsperadas: Seq[Data], caso: Int): Unit = {
      println(s"=== Caso $caso | $descricao ===")
      val (resultado, tempo) = medirTempo(f(datas, intervalos))
      println(s"Executado em $tempo ms")
      assert(
        datasEsperadas.length === resultado.length,
        s"Quantidade esperada: ${datasEsperadas.length} | Quantidade obtida: ${resultado.length}"
      )
      datasEsperadas.foreach(de => {
        assert(resultado.contains(de), s"Data esperada \"$de\" não encontrada")
      })
      println
    }

    testar(
      Seq(Data(2023, 1, 1), Data(2023, 1, 2)),
      Seq(Intervalo(Data(2023, 1, 1), Data(2023, 1, 1))),
      Seq(Data(2023, 1, 2)),
      1
    )

    testar(
      Seq(Data(2023, 1, 1), Data(2023, 1, 2)),
      Seq.empty[Intervalo],
      Seq(Data(2023, 1, 1), Data(2023, 1, 2)),
      2
    )

    testar(
      Seq(Data(2023, 4, 1)),
      Seq(Intervalo(Data(2023, 1, 1), Data(2023, 6, 1)), Intervalo(Data(2023, 2, 1), Data(2023, 3, 1))),
      Seq.empty[Data],
      3
    )

    testar(
      Seq(Data(2023, 1, 3)),
      Seq(Intervalo(Data(2023, 1, 1), Data(2023, 1, 2)), Intervalo(Data(2023, 1, 4), Data(2023, 1, 5))),
      Seq(Data(2023, 1, 3)),
      4
    )

    testar(
      Seq(Data(2023, 2, 1), Data(2023, 2, 2)),
      (1 to 30).map(i => Intervalo(Data(2023, 1, i), Data(2023, 1, i + 1))),
      Seq(Data(2023, 2, 1), Data(2023, 2, 2)),
      5
    )

    testar(
      (1 to 31).map(Data(2023, 1, _)),
      Seq(Intervalo(Data(2023, 1, 1), Data(2023, 1, 15)), Intervalo(Data(2023, 1, 20), Data(2023, 1, 27))),
      (16 to 19).map(Data(2023, 1, _)) ++ (28 to 31).map(Data(2023, 1, _)),
      6
    )

    val n = 5000
    testar(
      (1 to n).map(i => Data(2023, 1, 1).plusDays(i)),
      (1 to n).map { i =>
        val data = Data(2023, 1, 1).plusDays(i)
        Intervalo(data, data)
      },
      Seq.empty[Data],
      7
    )

    testar(
      Seq(Data(2023, 1, 1), Data(2023, 1, 5)),
      Seq(Intervalo(Data(2023, 1, 2), Data(2023, 1, 5)), Intervalo(Data(2023, 1, 2), Data(2023, 1, 3)), Intervalo(Data(2023, 1, 1), Data(2023, 1, 4))),
      Seq.empty[Data],
      8
    )

    testar(
      Seq(Data(2023, 1, 15), LocalDateTime.of(2023, 1, 15, 0, 0, 0, 1)),
      Seq(
        Intervalo(Data(2023, 1, 10), Data(2023, 1, 15)),
        Intervalo(Data(2023, 1, 11), Data(2023, 1, 15)),
        Intervalo(Data(2023, 1, 12), Data(2023, 1, 15)),
        Intervalo(Data(2023, 1, 13), Data(2023, 1, 15)),
        Intervalo(Data(2023, 1, 14), Data(2023, 1, 15)),
        Intervalo(Data(2023, 1, 15), Data(2023, 1, 15))
      ),
      Seq(LocalDateTime.of(2023, 1, 15, 0, 0, 0, 1)),
      9
    )

    testar(
      Seq(Data(2023, 1, 15), Data(2023, 1, 16)),
      Seq(Intervalo(Data(2023, 2, 1), Data(2023, 2, 10))),
      Seq(Data(2023, 1, 15), Data(2023, 1, 16)),
      10
    )

    testar(
      Seq(1, 4, 10, 7, 13, 12).map(Data(2023, 6, _)),
      Seq((2, 3), (5, 6), (8, 9), (11, 12)).map { case (a, b) => Intervalo(Data(2023, 6, a), Data(2023, 6, b)) },
      Seq(1, 4, 7, 10, 13).map(Data(2023, 6, _)),
      11
    )

    testar(Seq.empty[Data], Seq.empty[Intervalo], Seq.empty[Data], 12)

    testar(
      Seq(Data(2023, 1, 15), Data(2023, 1, 1)),
      Seq(
        Intervalo(Data(2023, 1, 1).plusSeconds(1), Data(2023, 1, 15).minusNanos(1)),
        Intervalo(Data(2023, 1, 15), Data(2023, 1, 15).plusNanos(1))
      ),
      Seq(Data(2023, 1, 1)),
      13
    )

    testar(
      Seq(Data(2024, 1, 1).minusNanos(1), Data(2023, 1, 15)),
      Seq(Intervalo(Data(2023, 12, 31), Data(2024, 1, 1))),
      Seq(Data(2023, 1, 15)),
      14
    )
  }

  // O(m log m + n)
  // Se m = n, O(m log m)
  it("deve filtrar datas em O(nm)") {
    testarFiltragemDeDatas(Solucao.filtrarDatasDentroDeIntervalosMLogMMaisN, "Solucao.filtrarDatasDentroDeIntervalosMN")
  }

  // O(n log m)
  it("deve filtrar datas em O(n log m)") {
    testarFiltragemDeDatas(Solucao.filtrarDatasDentroDeIntervalosMLogN, "Solucao.filtrarDatasDentroDeIntervalosMLogN")
  }
}
