/*
 * EchoService.java
 */
package ${package};

import com.sun.jbi.sample.component.common.Helper;
import java.io.StringReader;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * This interface defines the operations corresponding to the abstract wsdl
 * service definition defined for this echo service. The inner class EchoServiceImpl
 * implements the echo service operations which the service engine will invoke
 * when a InOut message exchange is processed to invoke a service provided by
 * the engine.
 *
 * @author chikkala
 */
public interface EchoService {
    /**
     * this operation takes the input xml document and constructs the output xml document
     * with the same data to implement the echo.
     * @param inputDoc input xml document
     * @return output xml document
     */
    Source echo(Source inputDoc) throws Exception ;
    /**
     * This class implements the echo service operations which the service engine
     * will invoke when a InOut message exchange is processed to invoke a service
     * provided by the engine.
     */
    public static class EchoServiceImpl implements EchoService {
        
        private EchoServiceImpl() {}
        /**
         * accessor to create/locate the service implementation
         * @return echo service implementation
         */
        public static EchoService getInstance() {
            return new EchoServiceImpl();
        }
        /**
         * this operation takes the input xml document and constructs the output xml document
         * with the same data to implement the echo.
         * @param inputDoc input document
         * @return output document
         */
        public Source echo(Source inputDoc) throws Exception {
            Document outDoc = null;
            StringBuffer inBuff = Helper.readFromSource(inputDoc);
            
            System.out.println("--------------- INPUT MESSAGE BEGIN -----------------");
            System.out.println(inBuff);
            System.out.println("--------------- INPUT MESSAGE END -------------------");
            
            outDoc = Helper.buildDOMDocument(new StringReader(inBuff.toString()));
            // find the value element and modify it to prefix "EchoResult:" text.
            NodeList nodes = outDoc.getElementsByTagName("value");
            Element ele = null;
            if ( nodes.getLength() > 0 ) {
                ele = (Element) nodes.item(0);
            }
            if ( ele != null ) {
                Node txtNode = outDoc.createTextNode("EchoResult: ");
                // prefix the txtNode.
                Node first = ele.getFirstChild();
                if ( first != null ) {
                    ele.insertBefore(txtNode, first);
                } else {
                    ele.appendChild(txtNode);
                }
            }
            DOMSource outSource = new DOMSource(outDoc);
            return outSource;
        }
    }
}
