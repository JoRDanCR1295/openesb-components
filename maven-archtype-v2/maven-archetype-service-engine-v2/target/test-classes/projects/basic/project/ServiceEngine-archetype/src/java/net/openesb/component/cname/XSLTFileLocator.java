/*
 * XSLTFileLocator.java
 */
package net.openesb.component.ServiceEngine-archetype;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import javax.xml.namespace.QName;

/**
 * This class represents the component specific deployment artifact which reads
 * the deployed "xsltmap.properties" file in the service unit and processes it
 * to find the xslt file to be used for a particular operation on a service
 * provided by the deployment.
 * <pre>
 * The "xsltmap.properties" file should contain two types of properties
 * 1.for namespace prefix to namespaceURI map that starts with "xmlns." prefix like
 *   "xmlns.tns-prefix=http://www.sun.com/jbi/examples/sample-service/echo"
 * 2. a service description to xslt file properties with syntax like
 *   "{tns-prefix}service-name=xslt_file_name"  and
 *   "{tns-prefix}service-name.{tns-prefix}operation-name=xslt_file_name" and
 *   "{tns-prefix}service-name.{tns-prefix}interface-name.operation-name=xslt_file_name" .
 * the service description property is a "." delimited tokens that represents a serive
 * or interface qname and a operation name. To locate xslt file either with service qname
 * or service qname and operaton or serivice qname and interface qname and operation,
 * all the three properties specified above should be there for each xslt file. for example,
 *  xmlns.echo1=http://www.sun.com/jbi/examples/sample-service/echo
 *  xmlns.echo2=http://www.sun.com/jbi/examples/sample-service/echo
 *  {echo2}echoService=echo.xsl
 *  {echo2}echoService.{echo1}echo=echo.xsl
 *  {echo2}echoService.{echo1}echo.echo=echo.xsl
 * </pre>
 *
 * @author chikkala
 */
public class XSLTFileLocator {
    
    private Map<String, String> mXsltFileMap;

    /**
     * Creates a new instance of XsltLocator
     */
    public XSLTFileLocator(String rootPath, String mapFile) throws Exception {
        this.mXsltFileMap = new HashMap<String, String>();
        initXsltLocator(rootPath, mapFile);
    }
    
    private void initXsltLocator(String rootPath, String mapFilePath) throws Exception {
        @SuppressWarnings("unchecked")
        Map<String, String> mapProps = loadMappingProperties(rootPath, mapFilePath);
        
        Map<String, String> nsMap = new HashMap<String, String>();
        Map<String, String> xsltMap = new HashMap<String, String>();
        
        for (String name : mapProps.keySet()) {
            String value = mapProps.get(name);
            if (name.startsWith("{")) {
                xsltMap.put(name, value);
            } else if (name.startsWith("xmlns.")) {
                String xmlns = name.substring(6);
                nsMap.put(xmlns, value);
            }
        }
        
        for (String name : xsltMap.keySet()) {
            String value = xsltMap.get(name);
            String[] tokens = name.split("\\.");
            String svcName = null;
            String intrName = null;
            String opName = null;
            
            if (tokens.length == 1) {
                svcName = expandQName(tokens[0], nsMap);
            } else if (tokens.length == 2) {
                svcName = expandQName(tokens[0], nsMap);
                opName = expandQName(tokens[1], nsMap);
            } else if (tokens.length == 3) {
                svcName = expandQName(tokens[0], nsMap);
                intrName = expandQName(tokens[1], nsMap);
                opName = tokens[2];
            } else {
                System.out.println("invalid property name in xslt map property" + name);
            }
            
            StringBuffer buff = new StringBuffer();
            buff.append(svcName);
            if (intrName != null) {
                buff.append(".");
                buff.append(intrName);
            }
            if (opName != null) {
                buff.append(".");
                buff.append(opName);
            }
            File file = new File(rootPath, value);
            String xsltFile = file.getAbsolutePath();
            this.mXsltFileMap.put(buff.toString(), xsltFile);
        }
    }
    
    private String expandQName(String qname, Map<String, String> xmlnsMap) {

        // qname passed is "{prefix}localname"
        QName temp = QName.valueOf(qname);
        String prefix = temp.getNamespaceURI();
        String nsURI = xmlnsMap.get(prefix);
        if (nsURI == null) {
            nsURI = prefix; // just use the prefix as it is.
        }
        QName realQName = new QName(nsURI, temp.getLocalPart());
        return realQName.toString();
    }
    
    private Map loadMappingProperties(String rootPath, String mapFilePath) throws Exception {
        File mapFile = new File(rootPath, mapFilePath);
        Properties mapProps = new Properties();
        if (!mapFile.exists()) {
            throw new Exception("Mapping file not found " + mapFilePath);
        }
        FileInputStream inStream = null;
        try {
            inStream = new FileInputStream(mapFile);
            mapProps.load(inStream);
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        } finally {
            if (inStream != null) {
                try {
                    inStream.close();
                } catch (IOException ex) {
                    ex.printStackTrace();
                }
            }
        }
        return mapProps;
    }

    /**
     * return the xslt file mapped to either service name or service qname +
     * operation qname
     *
     * @param serviceQName must be non null qname
     * @param opQName can be null the xslt file can be located at service level.
     */
    public String findXsltFile(QName serviceQName, QName opQName) {
        StringBuffer buff = new StringBuffer();
        buff.append(serviceQName.toString());
        if (opQName != null) {
            buff.append(".");
            // workaround for the http soap bc bug which does not set the namespace
            // uri on the op name. for time being use the serviceQName's namespace
            String namespaceURI = opQName.getNamespaceURI();
            if (namespaceURI == null || namespaceURI.length() == 0) {
                QName tempOp =
                        new QName(serviceQName.getNamespaceURI(), opQName.getLocalPart());
                buff.append(tempOp.toString());
            } else {
                buff.append(opQName.toString());
            }
        }
        String xsltFile = this.mXsltFileMap.get(buff.toString());
        return xsltFile;
    }

    /**
     * return the xslt file mapped to either service name or service qname +
     * interface qname + operation
     *
     * @param serviceQName must be non null service qname
     * @param intrQName interface qname
     * @param opName operation name ( not a qname)
     */
    public String findXsltFile(QName serviceQName, QName intrQName, String opName) {
        StringBuffer buff = new StringBuffer();
        buff.append(serviceQName.toString());
        if (intrQName != null) {
            buff.append(".");
            buff.append(intrQName.toString());
        }
        if (opName != null) {
            buff.append(".");
            buff.append(opName);
        }
        String xsltFile = this.mXsltFileMap.get(buff.toString());
        return xsltFile;
    }
    
    public String printMap() {
        StringWriter buff = new StringWriter();
        PrintWriter out = new PrintWriter(buff);
        for (String key : this.mXsltFileMap.keySet()) {
            out.println("Key= " + key);
            out.println("XsltFile= " + this.mXsltFileMap.get(key));
        }
        out.close();
        return buff.getBuffer().toString();
    }
}
