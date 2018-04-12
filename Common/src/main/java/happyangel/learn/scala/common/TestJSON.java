package happyangel.learn.scala.common;

/**
 * Created by happyangel on 18-2-7.
 */
public class TestJSON {
    private long helloWorld;


    public long getHelloWorld() {
        return helloWorld;
    }

    public void setHelloWorld(long helloWorld) {
       this.helloWorld = helloWorld;
    }

    @Override
    public String toString() {
        return helloWorld + "";
    }
}
