/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package StrucCvtDateTime;

import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;
import java.io.IOException;
import java.io.StringWriter;
import javax.xml.bind.JAXBException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Element;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.netbeans.xml.schema.cvtdatemessages.ElRequest;
import org.netbeans.xml.schema.cvtdatemessages.ElResponse;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author mczapski
 */
public class StrucCvtDateTime {

    static Object oBj = new Object();

    public static String CvtDateTime(String sReq) {

        Logger lgr = Logger.getLogger("StrucCvtDateTime");
        lgr.info("CvtDateTime input: " + sReq);
        String sRes = "";
        try {

            ElRequest elReq = jaxbUnmarshalFromString(sReq);
            lgr.info("unmarshalled request");
            ElResponse res = CvtDateTime(elReq);
            lgr.info("have response:" + res.getElDateString());
            sRes = jaxbMarshalToString(res);
            lgr.info("Have response string: " + sRes);
        } catch (IOException ex) {
            Logger.getLogger(StrucCvtDateTime.class.getName()).log(Level.SEVERE, null, ex);
        } catch (JAXBException ex) {
            Logger.getLogger(StrucCvtDateTime.class.getName()).log(Level.SEVERE, null, ex);
        } catch (Exception ex) {
            lgr.log(Level.SEVERE, "CvtDateTime", ex);
        }

        
        return sRes;
    }

    public static org.w3c.dom.Node CvtDateTime(org.w3c.dom.Node ndReq) {

        try {

            Logger lgr = Logger.getLogger("StrucCvtDateTime");

            lgr.log(Level.INFO, "Entered StrucCvtDateTime");
            lgr.log(Level.INFO, "StrucCvtDateTime TextContent " + ndReq.getTextContent());

            lgr.log(Level.INFO, "StrucCvtDateTime Content 2" + getPrettyDOMString(ndReq.getOwnerDocument(), true));

            Document doc = ndReq.getOwnerDocument();

            NodeList nl = doc.getElementsByTagName("elDateString");
            if (nl == null) {
                lgr.info("Null ...");
            }
            if (nl.getLength() <= 0) {
                lgr.info("There are 0 elements");
            }
            Node nd = nl.item(0);
            if (nd == null) {
                lgr.info("Item is null");
            }


            {
                NodeList sections = doc.getElementsByTagName("*");
                int numSections = sections.getLength();
                lgr.info("Num Sections: " + numSections);
                for (int i = 0; i < numSections; i++) {
                    Element section = (Element) sections.item(i); // A <sect1>
                    lgr.info("section name: " + section.getLocalName());

                    Node title = section.getFirstChild();
                    while (title != null && title.getNodeType() != Node.ELEMENT_NODE) {
                        lgr.info("Title: " + title.getLocalName());
                        title = title.getNextSibling();
                    }
                    if (title != null) {
                        lgr.info(title.getLocalName() + ":" + title.getFirstChild().getNodeValue());
                    }
                }
            }

            {
                lgr.info("-----------------------------------------------");
                NodeList sections = doc.getElementsByTagName("elRequest");
                int numSections = sections.getLength();
                lgr.info("Num Sections: " + numSections);
                for (int i = 0; i < numSections; i++) {
                    Element section = (Element) sections.item(i); // A <sect1>
                    lgr.info("section name: " + section.getLocalName());

                    Node title = section.getFirstChild();
                    while (title != null && title.getNodeType() != Node.ELEMENT_NODE) {
                        lgr.info("Title: " + title.getLocalName());
                        title = title.getNextSibling();
                    }
                    if (title != null) {
                        lgr.info(title.getLocalName() + ":" + title.getFirstChild().getNodeValue());
                    }
                }
            }


            ElRequest req = null;
            lgr.log(Level.INFO, "Req 1");
//        javax.xml.bind.JAXBContext jaxbCtx = javax.xml.bind.JAXBContext.newInstance(ElRequest.class.getPackage().getName());
            javax.xml.bind.JAXBContext jaxbCtx = javax.xml.bind.JAXBContext.newInstance("org.netbeans.xml.schema.cvtdatemessages");
            lgr.log(Level.INFO, "Req 2");
            javax.xml.bind.Unmarshaller unmarshaller = jaxbCtx.createUnmarshaller();
            lgr.log(Level.INFO, "Req 3");
            req = (ElRequest) unmarshaller.unmarshal(ndReq);
            lgr.log(Level.INFO, "Req 4");

            lgr.log(Level.INFO, "Req Date: " + req.getElDateString());
            lgr.log(Level.INFO, "Req InFMT: " + req.getElFormatIn());
            lgr.log(Level.INFO, "Req OutFMT: " + req.getElFormatOut());

            ElResponse res = CvtDateTime(req);

            lgr.log(Level.INFO, "Res Date: " + res.getElDateString());

            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Node node = builder.newDocument();

            javax.xml.bind.Marshaller marshaller = jaxbCtx.createMarshaller();
//        javax.xml.bind.JAXBContext jaxbCtx = javax.xml.bind.JAXBContext.newInstance(jaxbObj.getClass().getPackage().getName());
            marshaller.setProperty(javax.xml.bind.Marshaller.JAXB_ENCODING, "UTF-8");
            //NOI18N
            marshaller.setProperty(javax.xml.bind.Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
            marshaller.marshal(res, node);

            lgr.log(Level.INFO, "node: " + node.getTextContent());
            lgr.log(Level.INFO, "node: " + getPrettyDOMString((Document)node, true));
            
            return ((Document)node).getDocumentElement();

        } catch (ParserConfigurationException ex) {
            Logger.getLogger(StrucCvtDateTime.class.getName()).log(Level.SEVERE, null, ex);
        } catch (JAXBException ex) {
            Logger.getLogger(StrucCvtDateTime.class.getName()).log(Level.SEVERE, null, ex);
        } catch (Exception ex) {
            Logger.getLogger(StrucCvtDateTime.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }

    public static ElResponse CvtDateTime(ElRequest stReq) {

        java.text.DateFormat fmtIn = new java.text.SimpleDateFormat(stReq.getElFormatIn());
        java.text.DateFormat fmtOut = new java.text.SimpleDateFormat(stReq.getElFormatOut());

        String sDT = "";
        java.util.Date dt = null;
        synchronized (oBj) {
            try {
                dt = fmtIn.parse(stReq.getElDateString());
                sDT = fmtOut.format(dt);
            } catch (java.text.ParseException ex) {
            }
        }

        ElResponse sMsgOut = new ElResponse();
        sMsgOut.setElDateString(sDT);
        return sMsgOut;

    }

    public static String getPrettyDOMString(org.w3c.dom.Document docDocument, boolean blKeepSpace) throws IOException {

        StringWriter stringOut = new StringWriter();        //Writer will be a String
        OutputFormat format = new OutputFormat(docDocument, "UTF-8", true);
        format.setLineSeparator("\n");
        format.setPreserveSpace(blKeepSpace);
        format.setIndent(2);
        format.setLineWidth(76);

        XMLSerializer serial = new XMLSerializer(stringOut, format);
        serial.asDOMSerializer();                            // As a DOM Serializer
        serial.serialize(docDocument.getDocumentElement());
        return stringOut.toString(); //Spit out DOM as a String
    }

    private static ElRequest jaxbUnmarshalFromString(String str) throws javax.xml.bind.JAXBException {
        ElRequest ret = null;
        String packageName = "org.netbeans.xml.schema.cvtdatemessages"; //StrucCvtDateTime"; //ElRequest.class.getPackage().getName();
        javax.xml.bind.JAXBContext jaxbCtx = javax.xml.bind.JAXBContext.newInstance(packageName);
        javax.xml.bind.Unmarshaller unmarshaller = jaxbCtx.createUnmarshaller();
        ret = (ElRequest) unmarshaller.unmarshal(new java.io.StringReader(str));
        //NOI18N
        return ret;
    }

    private static String jaxbMarshalToString(ElResponse jaxbObj) throws javax.xml.bind.JAXBException, java.io.IOException {
        java.io.StringWriter sw = new java.io.StringWriter();
        javax.xml.bind.JAXBContext jaxbCtx = javax.xml.bind.JAXBContext.newInstance(jaxbObj.getClass().getPackage().getName());
        javax.xml.bind.Marshaller marshaller = jaxbCtx.createMarshaller();
        marshaller.setProperty(javax.xml.bind.Marshaller.JAXB_ENCODING, "UTF-8");
        //NOI18N
        marshaller.setProperty(javax.xml.bind.Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        marshaller.marshal(jaxbObj, sw);
        sw.close();
        return sw.toString();
    }
}
