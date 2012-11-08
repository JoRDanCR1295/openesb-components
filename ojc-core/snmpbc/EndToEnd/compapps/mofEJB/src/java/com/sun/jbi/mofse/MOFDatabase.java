/*
 * MOFDatabase.java
 *
 * Created on March 6, 2007, 3:15 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.mofse;

import com.sun.soabi.snmpbc.metadataquery.NetworkElementIdent;
import com.sun.soabi.snmpbc.metadataresponse.NetworkElementProperties;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * file implementation of MOF DB
 *
 * @author echou
 */
public class MOFDatabase {
    
    private static final String DB_XML = "com/sun/jbi/mofse/db.xml";
            
    private static MOFDatabase instance;
    
    private ArrayList<Metadata> db;
    
    public static synchronized MOFDatabase getInstance() throws Exception {
        if (instance == null) {
            instance = new MOFDatabase();
        }
        return instance;
    }
    
    /** Creates a new instance of MOFDatabase */
    private MOFDatabase() throws Exception {
        // init DB here
        db = new ArrayList<Metadata> ();
        
        // read db.xml to populate data
        InputStream is = MOFDatabase.class.getClassLoader().getResourceAsStream(DB_XML);
        if (is == null) {
            throw new Exception("unable to find db.xml as resource");
        }
        
        // parse
        DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        Document doc = null;
        try {
            doc = builder.parse(is);
        } finally {
            try {
                is.close();
            } catch (Exception e) {
            }
        }
        
        
        Element root = doc.getDocumentElement();
        NodeList networkElems = root.getElementsByTagName("NetworkElement");
        for (int i = 0; i < networkElems.getLength(); i++) {
            Element networkElem = (Element) networkElems.item(i);
            Metadata dataEntry = new Metadata();
            Element ipAddress = (Element) networkElem.getElementsByTagName("IPAddress").item(0);
            dataEntry.ipAddress = ipAddress.getTextContent();
            Element port = (Element) networkElem.getElementsByTagName("port").item(0);
            dataEntry.port = Integer.parseInt(port.getTextContent());
            NodeList processorIDs = networkElem.getElementsByTagName("ProcessorID");
            for (int j = 0; j < processorIDs.getLength(); j++) {
                Element processorID = (Element) processorIDs.item(j);
                dataEntry.processorIDs.add(processorID.getTextContent());
            }
            db.add(dataEntry);
        }
        
    }
    
    public NetworkElementProperties lookupNetworkElement(NetworkElementIdent networkElemIdent) {
        if (db.size() == 0) {
            return null;
        }
        
        int hashcode = (networkElemIdent.getIPAddress() + ":" +
                networkElemIdent.getPort().toString()).hashCode();
        int index = Math.abs(hashcode % db.size());
        Metadata metadata = db.get(index);
        if (metadata != null) {
            NetworkElementProperties networkElemProps = new NetworkElementProperties();
            networkElemProps.setReplyID(networkElemIdent.getReplyID());
            networkElemProps.getProcessorID().addAll(metadata.processorIDs);
            
            return networkElemProps;
        }
        
        return null;
    }
    
    class Metadata {
        String ipAddress = null;
        int port = 0;
        List<String> processorIDs = new ArrayList<String> ();
    }
}
