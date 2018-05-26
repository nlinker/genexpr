package meta.cb;

import java.lang.reflect.Method;
import java.util.Arrays;

import scala.collection.immutable.Map;

import static meta.cb.TestEntities.*;

public class BuildableJavaRun {

    public static void main(String[] args) throws Exception {
        SimpleCaseClass scc = new SimpleCaseClass(42, "something");
        System.out.println("scc = " + scc);
        // in groovy
        // import meta.cb.TestEntities
        // scc = new TestEntities.SimpleCaseClass(42, "something")
        // sccO = meta.cb.TestEntities$SimpleCaseClass$.MODULE$
        // sccO.toMap().apply(scc)
        Name$ name$ = Name$.MODULE$;
        Name name = new Name(name$.apply$default$1(), name$.apply$default$2());
        User$ user$ = User$.MODULE$;
        User user = new User(name, 33);
        meta.cb.ToMap<User> tm = (ToMap<User>) user$.getClass()
            .getDeclaredMethod("toMap").invoke(user$);
        Map<String, Object> m = tm.apply(user);
        System.out.println("m = " + m);

        // Method[] ms0 = sccO.getDeclaredMethods();
        // Arrays.stream(ms).forEach(System.out::println);
    }

}
