import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;
import java.util.Scanner;

class jLCT {
    private long cLCT;

    private native void jinitLCT(int n);
    private native int jsizeofStruct();
    private native long jallocLCT(int n);
    private native void jfreeLCT();

    public native int jAccess(int v);
    public native int jgetNode(int v, int d);
    public native boolean jedgeQ(int u, int v);
    public native int jcut(int u, int v);
    public native void jreRoot(int v);
    public native int jLCA(int u, int v);
    public native void jLink(int u, int v);
    public native double jsetCost(int u, double w);
    public native double jgetCost(int u, int v);
    public native void jupdate(int v, double w);
    public native int jgetMin(int v);

    static {
	System.loadLibrary("SplayLCT");
	// System.loadLibrary("PointerLCT");
    }

    jLCT(int n){
	this.cLCT = jallocLCT(n);
    }

    // @Override
    // protected void finalize() {
    // 	// System.out.println("Calling finalize");
    // 	this.jfreeLCT();
    // }

    public static void main(String[] args) {
	jLCT F = null;
	Scanner scan = new Scanner(System.in);

	int lnCt = 0; // Line Counter
	while(scan.hasNext()){
	    lnCt++;
	    String line = scan.nextLine();
	    Scanner scanToks = new Scanner(line);

	    if('#' == line.charAt(0)) continue;

	    System.out.printf("# %d : %s\n", lnCt, line);

	    String tok = scanToks.next();
	    if("allocLCT".equals(tok)){
		F = new jLCT(scanToks.nextInt());
		continue;
	    }

	    if("Access".equals(tok)){
		System.out.println(F.jAccess(scanToks.nextInt()));
		continue;
	    }

	    if("getNode".equals(tok)){
		System.out.println(F.jgetNode(scanToks.nextInt(), scanToks.nextInt()));
		continue;
	    }

	    if("edgeQ".equals(tok)){
		System.out.printf("%s\n", F.jedgeQ(scanToks.nextInt(), scanToks.nextInt()) ? "True" : "False");
		// System.out.println(F.jedgeQ(scanToks.nextInt(), scanToks.nextInt()));
		continue;
	    }

	    if("cut".equals(tok)){
		System.out.println(F.jcut(scanToks.nextInt(), scanToks.nextInt()));
		continue;
	    }

	    if("reRoot".equals(tok)){
		F.jreRoot(scanToks.nextInt());
		continue;
	    }

	    if("LCA".equals(tok)){
		System.out.println(F.jLCA(scanToks.nextInt(), scanToks.nextInt()));
		continue;
	    }

	    if("Link".equals(tok)){
		F.jLink(scanToks.nextInt(), scanToks.nextInt());
		continue;
	    }

	    if("setCost".equals(tok)){
		System.out.printf("%f\n", F.jsetCost(scanToks.nextInt(), scanToks.nextDouble()));
		continue;
	    }

	    if("getCost".equals(tok)){
		System.out.printf("%f\n", F.jgetCost(scanToks.nextInt(), scanToks.nextInt()));
		continue;
	    }

	    if("update".equals(tok)){
		F.jupdate(scanToks.nextInt(), scanToks.nextDouble());
		continue;
	    }

	    if("getMin".equals(tok)){
		int u = F.jgetMin(scanToks.nextInt());
		int v = F.jgetNode(u, -1);
		System.out.printf("%f\n", F.jgetCost(u, v));
		continue;
	    }
	}
	// F.finalize();
    }
}
