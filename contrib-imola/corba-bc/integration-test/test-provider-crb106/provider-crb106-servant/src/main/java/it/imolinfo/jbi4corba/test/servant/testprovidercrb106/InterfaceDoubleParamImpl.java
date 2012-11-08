package it.imolinfo.jbi4corba.test.servant.testprovidercrb106;

import it.imolinfo.jbi4corba.test.testprovidercrb106.StructAB;
import it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeA;
import it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeB;
import it.imolinfo.jbi4corba.test.testprovidercrb106.doubleparam.InterfaceDoubleParamPOA;

import java.util.logging.Logger;

public class InterfaceDoubleParamImpl extends InterfaceDoubleParamPOA {

    /**
     * logger.
     */
    private static Logger log = Logger.getLogger(InterfaceDoubleParamImpl.class.getName());
    
    // InterfaceDoubleParamPOA
    public StructAB methodA(ValueTypeA a, ValueTypeB b) {
        
      log.info("InterfaceDoubleParamPOA.methodA(ValueTypeA=" + a
        + "; ValueTypeB=" + b);
      System.out.println("InterfaceDoubleParamPOA.methodA(ValueTypeA=" + a
        + "; ValueTypeB=" + b);

      StructAB ab = new StructAB();
      ab.a = a;
      ab.b = b;

      log.info("InterfaceDoubleParamPOA.methodA - Output - StructAB="
        + ab);
      System.out.println("InterfaceDoubleParamPOA.methodA - Output - StructAB="
        + ab);
      return ab;
    }

    // InterfaceDoubleParamPOA
    public StructAB methodB(ValueTypeA a, ValueTypeB b) {
        
      log.info("InterfaceDoubleParamPOA.methodB(ValueTypeA=" + a
        + "; ValueTypeB=" + b);
      System.out.println("InterfaceDoubleParamPOA.methodB(ValueTypeA=" + a
        + "; ValueTypeB=" + b);

      StructAB ab = new StructAB();
      ab.a = a;
      ab.b = b;

      log.info("InterfaceDoubleParamPOA.methodB - Output - StructAB="
        + ab);
      System.out.println("InterfaceDoubleParamPOA.methodB - Output - StructAB="
        + ab);
      return ab;
    }

}
