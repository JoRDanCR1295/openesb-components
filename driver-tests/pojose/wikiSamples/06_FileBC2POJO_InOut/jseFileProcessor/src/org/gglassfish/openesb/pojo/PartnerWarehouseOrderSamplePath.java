package org.gglassfish.openesb.pojo;

import java.util.Date;

import org.netbeans.modules.soa.tedmet.core.DomUtil.Path;
import org.netbeans.modules.soa.tedmet.core.DomUtil.NodeContainer;
import org.netbeans.modules.soa.tedmet.core.Tedmet;

import org.w3c.dom.Document;

import org.w3c.dom.Element;

import org.w3c.dom.Node;

@Tedmet(sample="C:/tmp/pojo/tutorial/6_FileBC2POJO/jseFileProcessor/test/org/gglassfish/openesb/pojo/PartnerWarehouseOrderSample.xml")
public class PartnerWarehouseOrderSamplePath
    implements NodeContainer {
    public final Node mNode;

    public PartnerWarehouseOrderSamplePath (Node node) {
        if (node == null) {
            throw new NullPointerException("no node");
        }
        mNode = node;
    }

    // Required by the NodeContainer interface.
    public Node getNode () { return mNode; }

    /** @deprecated */
    public boolean FIXME (boolean value) { return value; }

    /** @deprecated */
    public int FIXME (int value) { return value; }

    /** @deprecated */
    public String FIXME (String value) { return value; }

    /**
     * Path: /WMS_Order/Header/Order_Number/TEXT,
     * example: "x000001".
     */
    public Path<String> order_Number = 
        new Path<String>(this, "/WMS_Order/Header/Order_Number/");

    /**
     * Path: /WMS_Order/Header/Order_Status_Code/TEXT,
     * example: "New".
     */
    public Path<String> order_Status_Code = 
        new Path<String>(this, "/WMS_Order/Header/Order_Status_Code/");

    /**
     * Path: /WMS_Order/Header/Site_Code/TEXT,
     * example: "Store1Warehouse1".
     */
    public Path<String> site_Code = 
        new Path<String>(this, "/WMS_Order/Header/Site_Code/");

    /**
     * Path: /WMS_Order/Header/Create_Date/TEXT,
     * example: "2/19/2003".
     */
    public Path<String> create_Date = 
        new Path<String>(this, "/WMS_Order/Header/Create_Date/");

    /**
     * Path: /WMS_Order/Header/Consignee_Address/Addr1/TEXT,
     * example: "SeeBeyond Inc".
     */
    public Path<String> addr1 = 
        new Path<String>(this, "/WMS_Order/Header/Consignee_Address/Addr1/");

    /**
     * Path: /WMS_Order/Header/Consignee_Address/Addr2/TEXT,
     * example: "800 E. Royal Oak Blvd, Monrovia, CA 91016".
     */
    public Path<String> addr2 = 
        new Path<String>(this, "/WMS_Order/Header/Consignee_Address/Addr2/");

    /**
     * Path: /WMS_Order/Header/Consignee_Address/Addr3/TEXT,
     * example: "USA".
     */
    public Path<String> addr3 = 
        new Path<String>(this, "/WMS_Order/Header/Consignee_Address/Addr3/");

    /**
     * Path: /WMS_Order/Header/Consignee_Address/TEXT.
     */
    public Path<String> consignee_Address = 
        new Path<String>(this, "/WMS_Order/Header/Consignee_Address/");

    /**
     * Path: /WMS_Order/Header/BOM_Type/TEXT,
     * example: "T0001".
     */
    public Path<String> BOM_Type = 
        new Path<String>(this, "/WMS_Order/Header/BOM_Type/");

    /**
     * Path: /WMS_Order/Header/TEXT.
     */
    public Path<String> header = 
        new Path<String>(this, "/WMS_Order/Header/");

    /**
     * Path: /WMS_Order/Line_Item%.
     */
    public Path<Node> line_Item = 
        new Path<Node>(this, "/WMS_Order/Line_Item%");

    /** Fetches fully indexed path, for DomUtil.set() and get(). */
    public Path<Node> line_Item (int i0) {
        return new Path<Node>(this, 
            "/WMS_Order/Line_Item=",
            i0);
    }

    /**
     * Path: /WMS_Order/Line_Item%/Counter/TEXT,
     * example: "1".
     */
    public Path<String> counter = 
        new Path<String>(this, "/WMS_Order/Line_Item%/Counter/");

    /** Fetches fully indexed path, for DomUtil.set() and get(). */
    public Path<String> counter (int i0) {
        return new Path<String>(this, 
            "/WMS_Order/Line_Item=/Counter/",
            i0);
    }

    /**
     * Path: /WMS_Order/Line_Item%/ItemCode/TEXT,
     * example: "SKU1".
     */
    public Path<String> itemCode = 
        new Path<String>(this, "/WMS_Order/Line_Item%/ItemCode/");

    /** Fetches fully indexed path, for DomUtil.set() and get(). */
    public Path<String> itemCode (int i0) {
        return new Path<String>(this, 
            "/WMS_Order/Line_Item=/ItemCode/",
            i0);
    }

    /**
     * Path: /WMS_Order/Line_Item%/Qty/TEXT,
     * example: "10".
     */
    public Path<String> qty = 
        new Path<String>(this, "/WMS_Order/Line_Item%/Qty/");

    /** Fetches fully indexed path, for DomUtil.set() and get(). */
    public Path<String> qty (int i0) {
        return new Path<String>(this, 
            "/WMS_Order/Line_Item=/Qty/",
            i0);
    }

    /**
     * Path: /WMS_Order/Line_Item%/Cost/TEXT,
     * example: "500.0".
     */
    public Path<String> cost = 
        new Path<String>(this, "/WMS_Order/Line_Item%/Cost/");

    /** Fetches fully indexed path, for DomUtil.set() and get(). */
    public Path<Float> cost (int i0) {
        return new Path<Float>(this, 
            "/WMS_Order/Line_Item=/Cost/",
            i0);
    }

    /**
     * Path: /WMS_Order/Line_Item%/TEXT.
     */
    public Path<String> line_Item_0 = 
        new Path<String>(this, "/WMS_Order/Line_Item%/");

    /** Fetches fully indexed path, for DomUtil.set() and get(). */
    public Path<String> line_Item_0 (int i0) {
        return new Path<String>(this, 
            "/WMS_Order/Line_Item=/",
            i0);
    }

    /**
     * Path: /WMS_Order/TEXT.
     */
    public Path<String> WMS_Order = 
        new Path<String>(this, "/WMS_Order/");

}
