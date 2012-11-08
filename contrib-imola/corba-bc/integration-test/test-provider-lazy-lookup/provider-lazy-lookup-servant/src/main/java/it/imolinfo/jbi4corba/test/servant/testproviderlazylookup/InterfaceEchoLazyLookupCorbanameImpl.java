package it.imolinfo.jbi4corba.test.servant.testproviderlazylookup;

import it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupCorbanamePOA;
import java.util.logging.Logger;

public class InterfaceEchoLazyLookupCorbanameImpl extends InterfaceEchoLazyLookupCorbanamePOA {

    /**
     * logger.
     */
    private static Logger log = Logger.getLogger(InterfaceEchoLazyLookupCorbanameImpl.class.getName());

    public String echo(String msg) {
        //throw new UnsupportedOperationException("Not supported yet.");
        log.info("[ EchoLazyLookupCorbaname ] echo(String) - message received: " + msg);
        return msg;
    }
}