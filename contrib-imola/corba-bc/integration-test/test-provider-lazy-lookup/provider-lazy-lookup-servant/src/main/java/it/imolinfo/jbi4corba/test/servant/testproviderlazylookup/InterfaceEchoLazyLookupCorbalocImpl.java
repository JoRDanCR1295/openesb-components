package it.imolinfo.jbi4corba.test.servant.testproviderlazylookup;

import it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbalocPOA;
import java.util.logging.Logger;

public class InterfaceEchoLazyLookupCorbalocImpl extends InterfaceEchoLazyLookupCorbalocPOA {

    /**
     * logger.
     */
    private static Logger log = Logger.getLogger(InterfaceEchoLazyLookupCorbalocImpl.class.getName());

    public String echo(String msg) {
        //throw new UnsupportedOperationException("Not supported yet.");
        log.info("[ EchoLazyLookupCorbaloc ] echo(String) - message received: " + msg);
        return msg;
    }
}