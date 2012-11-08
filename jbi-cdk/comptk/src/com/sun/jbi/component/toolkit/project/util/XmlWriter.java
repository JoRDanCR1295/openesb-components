/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)XmlWriter.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;

/**
 * 
 * @author Kevan Simpson
 */
public class XmlWriter {
    private static final String STR_UTF_8 = "UTF-8";
    private static TransformerFactory mFactory = TransformerFactory.newInstance();

    private Templates mXsl;
    private boolean mOmitDecl;
    
    public XmlWriter(boolean omitDeclaration) {
        mOmitDecl = omitDeclaration;
    }
    
    public String write(Node node) {
        if (node == null) return "<null/>";
        
        StringWriter writer = new StringWriter();
        try {
            writeXml(node, writer);
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new ProjectException(I18n.loc(    // XXX
                    "Failed to write node to string: {0}", e.getMessage()), e);
        }
        return writer.toString();
    }
    
    public void writeFile(Node node, File file) {
        if (node == null || file == null) return;
        
        FileOutputStream fos = null;
        OutputStreamWriter writer = null;
        
        try {
            fos = new FileOutputStream(file);
            writer = new OutputStreamWriter(fos, STR_UTF_8);
            node.normalize();
            writeXml(node, writer);
        }
        catch (Exception e) {
            throw new ProjectException(I18n.loc(    // XXX
                    "Failed to write node to file - {0}: {1}", 
                    file.getAbsolutePath(), e.getMessage()), e);
        }
        finally {
            Util.safeClose(writer);
        }
    }
    
    protected void writeXml(Node node, Writer writer) throws Exception {
        if (mXsl == null) {
            synchronized (mFactory) {
                mXsl = mFactory.newTemplates(new StreamSource(
                        XmlWriter.class.getResourceAsStream("pretty-print.xsl")));
            }
        }
        
//        Processor proc = new Processor(false);
//        XsltExecutable exec = proc.newXsltCompiler().compile(new StreamSource(
//                XmlWriter.class.getResourceAsStream("pretty-print.xsl")));
//        XsltTransformer tr = exec.load();
//        tr.setParameter(QName.fromClarkName("indent"), new XdmAtomicValue("    "));
//        tr.setParameter(QName.fromClarkName("newline"), new XdmAtomicValue("\n"));
//
//        Serializer out = new Serializer();
//        out.setOutputWriter(writer);
//        tr.setSource(new DOMSource(node));
//        tr.setDestination(out);
//        tr.transform();

        Transformer tr = mXsl.newTransformer();
        tr.setParameter("indent", "    ");  // 4 spaces
        tr.setParameter("newline", "\n");   // TODO replace w/ sys prop?
        tr.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, 
                             mOmitDecl ? "yes" : "no");
        StreamResult result = new StreamResult(writer);
        tr.transform(new DOMSource(node), result);
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        try {
            Document doc = XmlUtil.readXml(new File(
                    "C:\\openesb\\open-jbi-components\\ojc-core\\pom.xml"));
            XmlWriter xml = new XmlWriter(false);
            File out = new File(
                    "C:\\openesb\\open-jbi-components\\ojc-core\\test.xml");
//            xml.writeFile(doc.getDocumentElement(), out);
//            StreamResult result = new StreamResult(out);
//            Transformer tr = xml.tr();
//            tr.setParameter("indent", "    ");
//            tr.setParameter("newline", "\n");
//            
//            tr.transform(new DOMSource(doc.getDocumentElement()), result);
//            System.out.println(xml.write(doc.getDocumentElement()));
//            Util.writeFile(out, xml.out());
            xml.writeFile(doc.getDocumentElement(), out);
        }
        catch (Exception e) {
            e.printStackTrace();          
        }
    }

}
