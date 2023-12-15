/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Main.java to edit this template
 */
package jogodavelha;
import org.jpl7.*;
import java.util.Map;

/**
 *
 * @author hp
 */
public class JogoDaVelha {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
        
         // Consultar o arquivo Prolog
         Query consultQuery = new Query("consult('src/jogodavelha/exemplo.pl')");
        boolean consultSuccess = consultQuery.hasSolution(); // Consulta ao arquivo Prolog

        if (consultSuccess) {
            // Consulta para encontrar todos os filhos
            Query jogadas = new Query("melhor_jogada('O', ['X','O','_','X','X','_','_','_','_'], P)");
                Map<String, Term> solution = jogadas.nextSolution();
                System.out.println("Melhor Jogada = " + solution.get("P"));
            

            // Consulta para encontrar todas as mães
           /* Query maeQuery = new Query("mae(X, Y)");
            while (maeQuery.hasMoreSolutions()) {
                Map<String, Term> solution = maeQuery.nextSolution();
                System.out.println("Mãe: " + solution.get("X") + " do(a) filho(a) " + solution.get("Y"));
            }*/
        } else {
            System.out.println("Falha ao consultar o arquivo Prolog.");
        }
    }
    
}
