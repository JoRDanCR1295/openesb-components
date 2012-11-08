package it.imolinfo.jbi4corba.test.servant.testprovidercrb106;

import it.imolinfo.jbi4corba.test.testprovidercrb106.StructAB;
import it.imolinfo.jbi4corba.test.testprovidercrb106.ValueTypeA;
import it.imolinfo.jbi4corba.test.testprovidercrb106.singleparam.InterfaceSingleParamPOA;

import java.util.logging.Logger;

public class InterfaceSingleParamImpl extends InterfaceSingleParamPOA {

    /**
     * logger.
     */
    private static Logger log = Logger.getLogger(InterfaceSingleParamImpl.class.getName());

    // InterfaceSingleParamPOA
    public StructAB methodA(ValueTypeA a) {
        
        log.info("methodA - Input - ValueTypeA=" + a);

        StructAB ab = new StructAB();
        ab.a = a;
        ab.b = null;

        log.info("methodA - Output - StructAB=" + ab);
        return ab;
    }

    // InterfaceSingleParamPOA
    public StructAB methodB(ValueTypeA a) {
        
        log.info("methodB - Input - ValueTypeA=" + a);

        StructAB ab = new StructAB();
        ab.a = a;
        ab.b = null;
        
        log.info("methodB - Output - StructAB=" + ab);
        return ab;
    }

}
