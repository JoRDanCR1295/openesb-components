package it.imolinfo.jbi4corba.test.servant.testproviderlazylookup;

import it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupIORPOA;
import java.util.logging.Logger;

public class InterfaceEchoLazyLookupIORImpl extends InterfaceEchoLazyLookupIORPOA {

    /**
     * logger.
     */
    private static Logger log = Logger.getLogger(InterfaceEchoLazyLookupIORImpl.class.getName());

    public String echo(String msg) {
        //throw new UnsupportedOperationException("Not supported yet.");
        log.info("[ EchoLazyLookupIOR ] echo(String) - message received: " + msg);
        return msg;
    }
}