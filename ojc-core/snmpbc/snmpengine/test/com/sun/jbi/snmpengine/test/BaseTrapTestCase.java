/*
 * Created on Mar 2, 2007
 */
package com.sun.jbi.snmpengine.test;

import com.sun.jbi.snmpengine.SNMPCallback;
import com.sun.jbi.snmpengine.SNMPQueryInterceptor;
import com.sun.jbi.snmpengine.SNMPRA;
import com.sun.jbi.snmpengine.systest.TrapProducer;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.bind.JAXBContext;
import javax.xml.transform.Source;

import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import junit.framework.TestCase;

/**
 * Utilities for test cases
 * 
 * @author fkieviet
 */
public abstract class BaseTrapTestCase extends TestCase {

    private List<SNMPRA> mToDestroy = new ArrayList<SNMPRA>();
    private List<DatagramSocket> mToClose = new ArrayList<DatagramSocket>();
    private List<TrapProducer> mTrapProducersToClose = new ArrayList<TrapProducer>();

    @Override
    public void tearDown() {
        System.out.println("*** END OF " + getName());

        for (SNMPRA ra: mToDestroy) {
            ra.stop();
        }
        mToDestroy.clear();
        for (DatagramSocket s: mToClose) {
            s.close();
        }
        mToClose.clear();

        for (TrapProducer s: mTrapProducersToClose) {
            s.done();
        }
        mTrapProducersToClose.clear();
    }
    
    /**
     * @see junit.framework.TestCase#setUp()
     */
    @Override
    protected void setUp() throws Exception {
        System.out.println("*** START OF " + getName());
        super.setUp();
    }



    protected int pickPort() {
        return 50000;
    }

    protected void destroyOnExit(SNMPRA ra) {
        mToDestroy.add(ra);
    }

    protected void closeOnExit(DatagramSocket socket) {
        mToClose.add(socket);
    }

    protected void closeOnExit(TrapProducer p) {
        mTrapProducersToClose.add(p);
    }

    /**
     * Empty implementation for the SNMPCallback; makes it easier to add new methods to
     * the interface without having to change all existing tests
     * 
     * @author fkieviet
     */
    public class EmptyCallback implements SNMPCallback {
        public void deliverTraps(String batchId, String trapProcessorID, Source traps) {
        }

        public void getMetaData(String queryId, Source query) {
            System.out.println("getMetaData()... ignored");
        }

        public void replyPM(String requestId, Source response) {
            // TODO Auto-generated method stub
            
        }
    }
    
    public class QuerySimulator implements SNMPQueryInterceptor {
        public String[] getDestinations(InetAddress ipaddress, int port) {
            return new String[] {"A"};
        }
    };

    /**
     * A poor solution to printing a DOM
     * TODO replace
     * @param node
     */
    public static void printDOM(Node node) {
        int type = node.getNodeType();
        switch (type) {
        // print the document element
        case Node.DOCUMENT_NODE: {
            System.out.println("<?xml version=\"1.0\" ?>");
            printDOM(((Document) node).getDocumentElement());
            break;
        }
    
            // print element with attributes
        case Node.ELEMENT_NODE: {
            System.out.print("<");
            System.out.print(node.getNodeName());
            NamedNodeMap attrs = node.getAttributes();
            for (int i = 0; i < attrs.getLength(); i++) {
                Node attr = attrs.item(i);
                System.out.print(" " + attr.getNodeName().trim() + "=\""
                    + attr.getNodeValue().trim() + "\"");
            }
            System.out.println(">");
    
            NodeList children = node.getChildNodes();
            if (children != null) {
                int len = children.getLength();
                for (int i = 0; i < len; i++)
                    printDOM(children.item(i));
            }
    
            break;
        }
    
            // handle entity reference nodes
        case Node.ENTITY_REFERENCE_NODE: {
            System.out.print("&");
            System.out.print(node.getNodeName().trim());
            System.out.print(";");
            break;
        }
    
            // print cdata sections
        case Node.CDATA_SECTION_NODE: {
            System.out.print("<![CDATA[");
            System.out.print(node.getNodeValue().trim());
            System.out.print("]]>");
            break;
        }
    
            // print text
        case Node.TEXT_NODE: {
            System.out.print(node.getNodeValue().trim());
            break;
        }
    
            // print processing instruction
        case Node.PROCESSING_INSTRUCTION_NODE: {
            System.out.print("<?");
            System.out.print(node.getNodeName().trim());
            String data = node.getNodeValue().trim();
            {
                System.out.print(" ");
                System.out.print(data);
            }
            System.out.print("?>");
            break;
        }
        }
    
        if (type == Node.ELEMENT_NODE) {
            System.out.println();
            System.out.print("</");
            System.out.print(node.getNodeName().trim());
            System.out.print('>');
        }
    }

    public BaseTrapTestCase() {
        super();
    }
    
    private class Con extends ConsoleHandler {
        int k;
        public Con() {
            super();
            setLevel(Level.ALL);
        }
        
        public void publish(LogRecord r) {
            System.out.println(getName() + " "  + k++ + ": " + r.getMessage());
        }
    };

    
    public void setLogLevel(String name, Level level) {
        Logger logger = Logger.getLogger(name);
        logger.setLevel(level);
        boolean alreadyThere = false;
        for (Handler h : logger.getHandlers()) {
            if (h instanceof Con) {
                alreadyThere = true; 
            }
        }
        
        if (!alreadyThere) {
            logger.addHandler(new Con());
        }
    }
    

    public class Coder {
        final JAXBContext jaxbContext;
        final JAXBContext jaxbContextRequest;
        final JAXBContext jaxbContextReply;
        
        public Coder() throws Exception {
            jaxbContext = JAXBContext.newInstance("com.sun.soabi.snmpbc.traps", this.getClass().getClassLoader());
            jaxbContextRequest = JAXBContext.newInstance("com.sun.soabi.snmpbc.metadataquery", this.getClass().getClassLoader());
            jaxbContextReply = JAXBContext.newInstance("com.sun.soabi.snmpbc.metadataresponse", this.getClass().getClassLoader());
        }
    }

}