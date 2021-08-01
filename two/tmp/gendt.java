import java.io.*;

public class gendt {

	public static void main(String[] args) {
		FileOutputStream fos;
		PrintStream ps;

		try {
			int n = 10000;
			fos = new FileOutputStream ("dt.pl");
			ps = new PrintStream(fos);
			ps.println("% Deep taxonomy");
			ps.println("% See http://ruleml.org/WellnessRules/files/WellnessRulesN3-2009-11-10.pdf");
			ps.println("");
			ps.println("% type/2 comes from http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
			ps.println("type(z,class(n0)).");
			for (int i = 0; i < n; i++) {
				ps.println("type(X,class(n" + (i+1) + ")) :- type(X,class(n" + i +")).");
				ps.println("type(X,class(i" + (i+1) + ")) :- type(X,class(n" + i +")).");
				ps.println("type(X,class(j" + (i+1) + ")) :- type(X,class(n" + i +")).");
			}
			ps.println("");
			ps.println("% test cases");
			ps.println("case(type(_,class(n1))).");
			ps.println("case(type(_,class(n10))).");
			ps.println("case(type(_,class(n100))).");
			ps.println("case(type(_,class(n1000))).");
			ps.println("case(type(_,class(n10000))).");
			ps.println("");
			ps.println("test :-");
			ps.println("    case(A),");
			ps.println("    A,");
			ps.println("    write(A),");
			ps.println("    write('.'),");
			ps.println("    nl,");
			ps.println("    fail.");
			ps.println("test :-");
			ps.println("    halt.");
			fos.close();
		} catch (Throwable t) {
			t.printStackTrace();
		}
	}
}
