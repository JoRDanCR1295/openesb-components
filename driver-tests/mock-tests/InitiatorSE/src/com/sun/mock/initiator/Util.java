package com.sun.mock.initiator;

import java.io.File;
import java.io.InputStream;

import javax.wsdl.Definition;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;

import org.w3c.dom.Element;
import org.xml.sax.EntityResolver;

import com.ibm.wsdl.factory.WSDLFactoryImpl;

public class Util {

	public static Definition readWsdl(File f, EntityResolver resolver)  throws javax.wsdl.WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        Definition def = reader.readWSDL(f.getAbsolutePath());

        return def;
    }

	public static Definition readWsdl(String baseURL, Element el, EntityResolver resolver)  throws javax.wsdl.WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        Definition def = reader.readWSDL(baseURL, el);

        return def;
    }
}
