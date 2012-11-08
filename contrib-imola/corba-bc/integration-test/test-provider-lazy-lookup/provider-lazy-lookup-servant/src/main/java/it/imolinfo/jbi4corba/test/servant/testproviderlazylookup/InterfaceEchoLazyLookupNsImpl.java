package it.imolinfo.jbi4corba.test.servant.testproviderlazylookup;

import it.imolinfo.jbi4corba.test.testproviderlazylookup.InterfaceEchoLazyLookupNsPOA;
import java.util.logging.Logger;

public class InterfaceEchoLazyLookupNsImpl extends InterfaceEchoLazyLookupNsPOA {

    /**
     * logger.
     */
    private static Logger log = Logger.getLogger(InterfaceEchoLazyLookupNsImpl.class.getName());

    public String echo(String msg) {
        //throw new UnsupportedOperationException("Not supported yet.");
        log.info("[ EchoLazyLookupNs ] echo(String) - message received: " + msg);
        return msg;
    }
}
