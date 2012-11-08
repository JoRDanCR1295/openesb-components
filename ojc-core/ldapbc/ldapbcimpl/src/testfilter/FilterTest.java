/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package testfilter;

import com.sun.jbi.ldapbc.LDAPFilterGenerator;
import com.sun.jbi.ldapbc.LDAPSearch;
import com.sun.jbi.ldapbc.OutboundMessageProcessor;
import java.io.File;
import org.exolab.castor.xml.schema.Group;
import java.util.Enumeration;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.MessagingException;
import org.exolab.castor.xml.schema.AttributeDecl;
import org.exolab.castor.xml.schema.ComplexType;
import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.reader.SchemaReader;



/**
 *
 * @author tianlize
 */
public class FilterTest {

    private Schema readXsd(String fileName) throws Exception {
        File f = new File(fileName);
        if (!f.getName().toLowerCase().endsWith(".xsd")) {
            printMsg("not .xsd file!");
            return null;
        }
        SchemaReader reader = new SchemaReader(f);
        Schema schema = reader.read();
        return schema;
    }

    private void printMsg(String msg) {
        System.out.println(msg);
    }

    private LDAPSearch getLDAPSearch(String fileName) {
        Schema schema = null;
        try {
            schema = readXsd(fileName);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        if (schema == null) {
            return null;
        }
        LDAPSearch ret = new LDAPSearch();
        ComplexType searchFilterType = schema.getComplexType("SearchFilterType");
        Group group = (Group) searchFilterType.getParticle(0);
        if (group == null) {
            printMsg("no request sequence");
            return null;
        }
        int eleCount = group.getParticleCount();
        if (eleCount < 1) {
            printMsg("no request element");
            return null;
        }
        for (int l = 0; l < eleCount; l++) {
            ElementDecl element = (ElementDecl) group.getParticle(l);
            String eleName = element.getName();
            int index = eleName.indexOf(".");
            String objName = eleName.substring(0, index);
            String attName = eleName.substring(index + 1);
            int positionIndex = -1;
            int bracketDepth = -1;
            int bracketBeginDepth = -1;
            int bracketEndDepth = -1;
            String logicOp = "";
            String compareOp = "";
            Enumeration attrs = ((ComplexType) element.getType()).getAttributeDecls();
            while (attrs.hasMoreElements()) {
                AttributeDecl attr = (AttributeDecl) attrs.nextElement();
                String attrName = attr.getName();
                if (attrName.equals("positionIndex")) {
                    positionIndex = Integer.parseInt(attr.getFixedValue());
                    continue;
                }
                if (attrName.equals("bracketDepth")) {
                    bracketDepth = Integer.parseInt(attr.getFixedValue());
                    continue;
                }
                if (attrName.equals("bracketBeginDepth")) {
                    bracketBeginDepth = Integer.parseInt(attr.getFixedValue());
                    continue;
                }
                if (attrName.equals("bracketEndDepth")) {
                    bracketEndDepth = Integer.parseInt(attr.getFixedValue());
                    continue;
                }
                if (attrName.equals("logicOp")) {
                    logicOp = attr.getFixedValue();
                    continue;
                }
                if (attrName.equals("compareOp")) {
                    compareOp = attr.getFixedValue();
                }
            }
            ret.addFilter(positionIndex, objName, logicOp, attName, compareOp, bracketDepth, bracketBeginDepth, bracketEndDepth, attName + "Value");
         }  
        return ret;
    }
    
    private String getFilter(LDAPSearch search) {
        String ret = "";
        if(search==null){
            return "";
        }
        LDAPFilterGenerator filterGen = new LDAPFilterGenerator(search.getAttributes());
        try {
            ret += filterGen.generateFilter();
        } catch (MessagingException ex) {
            Logger.getLogger(OutboundMessageProcessor.class.getName()).log(Level.SEVERE, null, ex);
        }
        return ret;
    }

    public static void main(String[] args) {
        FilterTest filterTest = new FilterTest();
        LDAPSearch ldapSearch = filterTest.getLDAPSearch("D:\\dev\\nbproject\\AddBpelModule\\src\\ldapwsdls\\UserAdd.xsd");
        System.out.println(filterTest.getFilter(ldapSearch));
        ldapSearch = null;
    }
}
