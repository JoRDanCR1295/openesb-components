package com.sun.mock.participant1;

import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Element;
import org.xml.sax.EntityResolver;

import com.ibm.wsdl.factory.WSDLFactoryImpl;

public class Util {

	private static final Logger mLogger = Logger.getLogger(Util.class.getName());
	   
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
	
	public static Definition initDefinition() {
		Definition mDefinition = null;
		
    	try {
    		URL wsdl = ParticipantSE1LifeCycle.class.getResource("resources/transferAmount.wsdl");
    		InputStream wsdlIn = Util.class.getResourceAsStream("resources/transferAmount.wsdl");
    		Element wsdlEl = XmlUtil.loadElement(wsdlIn);
    		mDefinition = Util.readWsdl(wsdl.toString(), wsdlEl, null);
    	} catch(Exception ex) {
    		mLogger.log(Level.SEVERE, "wsdl is null", ex);
    	}
    	
    	return mDefinition;
    }
	
	public static Source createDummySource(Definition mDef) throws Exception {
        DOMSource source = null;
        InputStream in = Util.class.getResourceAsStream("resources/transferAmountRequest.xml");
        Element element = XmlUtil.loadElement(in);
        
//      convert result (ie. status) to xml element
        Map<String, Element> partsMap = new HashMap<String, Element>();
        partsMap.put(ServiceConstants.INPUT_PART_NAME, element);
        
        PortType pt = mDef.getPortType(new QName("http://j2ee.netbeans.org/wsdl/transferAmount", "transferAmountPortType"));
        Operation op = pt.getOperation("transferAmountOperation", "input1", "output1");
        
        Element result = JBIMessageUtil.makeJBIMessage(partsMap, op);


        source = new DOMSource(result);
        return source;
    }

}
