/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.gglassfish.openesb.pojo;

import org.glassfish.openesb.pojose.api.annotation.*;
import java.util.logging.Logger;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.netbeans.modules.soa.tedmet.core.DomUtil;
import org.openide.util.Exceptions;
import org.w3c.dom.Document;
import org.w3c.dom.Node;


/**
 *
 * @author gpatil
 */

@POJO (name="FileProcessor",interfaceQN="{http://j2ee.netbeans.org/wsdl/jseFileProcessor/FileProcessor}FileInboundPortType",serviceQN="{http://j2ee.netbeans.org/wsdl/jseFileProcessor/FileProcessor}FileInboundPortTypeService")
public class FileProcessor {
    // logger
    private static final Logger logger = Logger.getLogger(FileProcessor.class.getName());
    
    /**
     * Constructor
     */
    public FileProcessor() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation (outMessageTypeQN="{http://j2ee.netbeans.org/wsdl/jseFileProcessor/FileProcessor}PollOutputMessage")
    public Source receive(Source input) {   
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setValidating(false);
            Document tDoc = factory.newDocumentBuilder().newDocument();
            Document sDoc = null;
            if (input instanceof DOMSource){
                Node n = ((DOMSource)input).getNode();
                
                if (n.getNodeType() == Node.DOCUMENT_NODE){
                    sDoc = (Document)n;
                } else {
                    sDoc = factory.newDocumentBuilder().newDocument();
                    n = sDoc.importNode(n, true);
                    sDoc.appendChild(n);
                }

                NativeWarehouseOrderSampleToPartnerWarehouseOrderSample(sDoc, tDoc);                
                return new DOMSource(tDoc);
            }
        } catch (ParserConfigurationException ex) {
            Exceptions.printStackTrace(ex);
        }
        return input;        
    }

    public void NativeWarehouseOrderSampleToPartnerWarehouseOrderSample(Document sDoc, Document tDoc) {
        DomUtil du = new DomUtil();
        final NativeWarehouseOrderSamplePath sp = new NativeWarehouseOrderSamplePath(sDoc);
        final PartnerWarehouseOrderSamplePath tp = new PartnerWarehouseOrderSamplePath(tDoc);

        du.set(tp.order_Number, du.get(sp.purchaseID));
        // "x000001"
        du.set(tp.order_Status_Code, du.get(sp.order_Status));
        // "New"
        du.set(tp.site_Code, du.get(sp.store_Number) + du.get(sp.warehouse_Number));
        du.set(tp.create_Date, du.get(sp.order_Date));
        du.set(tp.addr1, du.get(sp.shipping_Address_1));
        // "SeeBeyond Inc"
        du.set(tp.addr2, du.get(sp.shipping_Address_2) + ", " + du.get(sp.shipping_Address_3));
        du.set(tp.addr3, du.get(sp.shipping_Address_4));
        // "USA"
        du.set(tp.BOM_Type, du.get(sp.FIXME("T0001")));

// Line_Item
        System.out.println("####Details elems:" + du.count(sp.detail));
        for (int i0 = 0, j0 = du.count(tp.line_Item); i0 < 2; i0++, j0++) {
            System.out.println("#### i0. j0" + i0 + ":" +j0 );
            du.set(tp.counter(j0), du.get(sp.line_Number(i0)));
            du.set(tp.itemCode(j0), du.get(sp.sku_Number(i0)));
            du.set(tp.qty(j0), du.get(sp.order_Quantity(i0)));
            du.set(tp.cost(j0), du.get(sp.retail_Price(i0)));
        }

// *** Remove start
//        du.set(tp.counter(1), du.get(sp.FIXME("2")));
//        du.set(tp.itemCode(1), du.get(sp.FIXME("SKU2")));
//        du.set(tp.qty(1), du.get(sp.FIXME("100")));
//        du.set(tp.cost(1), du.get(sp.FIXME("20.0")));
    }


}