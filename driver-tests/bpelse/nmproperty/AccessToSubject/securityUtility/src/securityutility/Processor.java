/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package securityutility;

import java.security.Principal;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.Subject;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 *
 * @author mpottlapelli
 */
public class Processor {

    private static DocumentBuilder builder;


    static {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setValidating(false);
        try {
            // Create the builder and parse the file
            builder = factory.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
            Logger.getLogger(Processor.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
    }

    public static Node extractSubject(Subject subj) {
        Document doc = builder.newDocument();
        Element nodePrincipals = doc.createElementNS("http://xml.netbeans.org/schema/securityInfo", "Principals");
        doc.appendChild(nodePrincipals);


        Set<Principal> principals = subj.getPrincipals();
        for (Principal principal : principals) {
            Element nodePrincipal = doc.createElementNS("http://xml.netbeans.org/schema/securityInfo", "Principal");
            nodePrincipals.appendChild(nodePrincipal);
            nodePrincipal.setTextContent(principal.getName());
        }
        return doc;
    }
}
