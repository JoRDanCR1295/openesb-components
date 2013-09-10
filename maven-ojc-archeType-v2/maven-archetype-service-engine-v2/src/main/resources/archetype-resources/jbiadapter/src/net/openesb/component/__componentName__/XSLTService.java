#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * XSLTService.java
 */
package net.openesb.component.${componentName};

import net.openesb.component.${componentName}.common.RuntimeHelper;
import java.io.File;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Properties;
import java.util.logging.Logger;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import org.w3c.dom.Document;

/**
 * This is a sample service implementation that takes the input as xml document
 * and the xslt file to apply the xslt transformation on the input xml and
 * return the transformed output xml. This implementation of the service is
 * invoked when a InOut message exchange is processed to invoke a service
 * provided by the engine.
 *
 * @author chikkala
 */
public interface XSLTService {

    /**
     * this operation takes the input xml document and constructs the output xml
     * document by applying the xslt in the file specified in xsltPath
     *
     * @param inputDoc input xml document
     * @param xsltPath absolute file path of xslt file.
     * @return transformed output xml document
     */
    Source transform(Source inputDoc, String xsltPath) throws Exception;

    /**
     * This class implements the echo service operations which the service
     * engine will invoke when a InOut message exchange is processed to invoke a
     * service provided by the engine.
     */
    public static class XSLTServiceImpl implements XSLTService {
        
        private XSLTServiceImpl() {
        }

        /**
         * accessor to create/locate the service implementation
         *
         * @return echo service implementation
         */
        public static XSLTService getInstance() {
            return new XSLTServiceImpl();
        }

        protected Logger getLogger() {
            return RuntimeHelper.getLogger();
        }

        /**
         * this operation takes the input xml document and constructs the output
         * xml document with the same data to implement the echo.
         *
         * @param inputDoc input document
         * @return output document
         */
        public Source transform(Source inputDoc, String xsltPath) throws Exception {
            Document outDoc = null;
            StringBuffer inBuff = RuntimeHelper.readFromSource(inputDoc);
            
            getLogger().fine("${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound} TRANSFORMER INPUT MESSAGE BEGIN ${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}");
            getLogger().fine(inBuff.toString());
            getLogger().fine("${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound} TRANSFORMER INPUT MESSAGE END  ${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}");
            
            TransformerFactory tf = null;
            tf = TransformerFactory.newInstance();
            StreamSource xsltSource = new StreamSource(new File(xsltPath));
            Transformer xformer = tf.newTransformer(xsltSource);
            
            Properties params = new Properties(); //TODO get the pramas from the user file
            for (Object name : params.keySet()) {
                xformer.setParameter((String) name, params.get(name));
            }
            StringWriter writer = new StringWriter();
            StreamResult result = new StreamResult(writer);
            
            DOMSource xmlSource = new DOMSource(RuntimeHelper.buildDOMDocument(new StringReader(inBuff.toString())));
            
            xformer.transform(xmlSource, result);
            
            getLogger().fine("${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound} TRANSFORMER OUTPUT MESSAGE BEGIN ${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}");
            getLogger().fine(writer.getBuffer().toString());
            getLogger().fine("${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound} TRANSFORMER OUTPUT MESSAGE END  ${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}${symbol_pound}");
            
            outDoc = RuntimeHelper.buildDOMDocument(new StringReader(writer.getBuffer().toString()));
            DOMSource outSource = new DOMSource(outDoc);
            
            return outSource;
        }
    }
}
