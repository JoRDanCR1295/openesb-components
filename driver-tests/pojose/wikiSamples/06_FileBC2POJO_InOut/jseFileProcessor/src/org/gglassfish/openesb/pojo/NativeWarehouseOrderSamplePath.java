package org.gglassfish.openesb.pojo;

import java.util.Date;

import org.netbeans.modules.soa.tedmet.core.DomUtil.Path;
import org.netbeans.modules.soa.tedmet.core.DomUtil.NodeContainer;
import org.netbeans.modules.soa.tedmet.core.Tedmet;

import org.w3c.dom.Document;

import org.w3c.dom.Element;

import org.w3c.dom.Node;

@Tedmet(sample="C:/tmp/pojo/tutorial/6_FileBC2POJO/jseFileProcessor/test/org/gglassfish/openesb/pojo/NativeWarehouseOrderSample.xml")
public class NativeWarehouseOrderSamplePath
    implements NodeContainer {
    public final Node mNode;

    public NativeWarehouseOrderSamplePath (Node node) {
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
     * Path: /RMS_WarehouseOrder/Header/PurchaseID/TEXT,
     * example: "x000001".
     */
    public Path<String> purchaseID = 
        new Path<String>(this, "/RMS_WarehouseOrder/Header/PurchaseID/");

    /**
     * Path: /RMS_WarehouseOrder/Header/Order_Status/TEXT,
     * example: "New".
     */
    public Path<String> order_Status = 
        new Path<String>(this, "/RMS_WarehouseOrder/Header/Order_Status/");

    /**
     * Path: /RMS_WarehouseOrder/Header/Store_Number/TEXT,
     * example: "Store1".
     */
    public Path<String> store_Number = 
        new Path<String>(this, "/RMS_WarehouseOrder/Header/Store_Number/");

    /**
     * Path: /RMS_WarehouseOrder/Header/Warehouse_Number/TEXT,
     * example: "Warehouse1".
     */
    public Path<String> warehouse_Number = 
        new Path<String>(this, "/RMS_WarehouseOrder/Header/Warehouse_Number/");

    /**
     * Path: /RMS_WarehouseOrder/Header/Order_Date/TEXT,
     * example: "2/19/2003".
     */
    public Path<String> order_Date = 
        new Path<String>(this, "/RMS_WarehouseOrder/Header/Order_Date/");

    /**
     * Path: /RMS_WarehouseOrder/Header/Required_Date/TEXT,
     * example: "2/19/2003".
     */
    public Path<String> required_Date = 
        new Path<String>(this, "/RMS_WarehouseOrder/Header/Required_Date/");

    /**
     * Path: /RMS_WarehouseOrder/Header/Shipping_Detail/Shipping_Address_1/TEXT,
     * example: "SeeBeyond Inc".
     */
    public Path<String> shipping_Address_1 = 
        new Path<String>(this, "/RMS_WarehouseOrder/Header/Shipping_Detail/Shipping_Address_1/");

    /**
     * Path: /RMS_WarehouseOrder/Header/Shipping_Detail/Shipping_Address_2/TEXT,
     * example: "800 E. Royal Oak Blvd".
     */
    public Path<String> shipping_Address_2 = 
        new Path<String>(this, "/RMS_WarehouseOrder/Header/Shipping_Detail/Shipping_Address_2/");

    /**
     * Path: /RMS_WarehouseOrder/Header/Shipping_Detail/Shipping_Address_3/TEXT,
     * example: "Monrovia, CA 91016".
     */
    public Path<String> shipping_Address_3 = 
        new Path<String>(this, "/RMS_WarehouseOrder/Header/Shipping_Detail/Shipping_Address_3/");

    /**
     * Path: /RMS_WarehouseOrder/Header/Shipping_Detail/Shipping_Address_4/TEXT,
     * example: "USA".
     */
    public Path<String> shipping_Address_4 = 
        new Path<String>(this, "/RMS_WarehouseOrder/Header/Shipping_Detail/Shipping_Address_4/");

    /**
     * Path: /RMS_WarehouseOrder/Header/Shipping_Detail/TEXT.
     */
    public Path<String> shipping_Detail = 
        new Path<String>(this, "/RMS_WarehouseOrder/Header/Shipping_Detail/");

    /**
     * Path: /RMS_WarehouseOrder/Header/TEXT.
     */
    public Path<String> header = 
        new Path<String>(this, "/RMS_WarehouseOrder/Header/");

    /**
     * Path: /RMS_WarehouseOrder/Detail%.
     */
    public Path<Node> detail = 
        new Path<Node>(this, "/RMS_WarehouseOrder/Detail%");

    /** Fetches fully indexed path, for DomUtil.set() and get(). */
    public Path<Node> detail (int i0) {
        return new Path<Node>(this, 
            "/RMS_WarehouseOrder/Detail=",
            i0);
    }

    /**
     * Path: /RMS_WarehouseOrder/Detail%/Line_Number/TEXT,
     * example: "1".
     */
    public Path<String> line_Number = 
        new Path<String>(this, "/RMS_WarehouseOrder/Detail%/Line_Number/");

    /** Fetches fully indexed path, for DomUtil.set() and get(). */
    public Path<String> line_Number (int i0) {
        return new Path<String>(this, 
            "/RMS_WarehouseOrder/Detail=/Line_Number/",
            i0);
    }

    /**
     * Path: /RMS_WarehouseOrder/Detail%/Sku_Number/TEXT,
     * example: "SKU1".
     */
    public Path<String> sku_Number = 
        new Path<String>(this, "/RMS_WarehouseOrder/Detail%/Sku_Number/");

    /** Fetches fully indexed path, for DomUtil.set() and get(). */
    public Path<String> sku_Number (int i0) {
        return new Path<String>(this, 
            "/RMS_WarehouseOrder/Detail=/Sku_Number/",
            i0);
    }

    /**
     * Path: /RMS_WarehouseOrder/Detail%/Order_Quantity/TEXT,
     * example: "10".
     */
    public Path<String> order_Quantity = 
        new Path<String>(this, "/RMS_WarehouseOrder/Detail%/Order_Quantity/");

    /** Fetches fully indexed path, for DomUtil.set() and get(). */
    public Path<String> order_Quantity (int i0) {
        return new Path<String>(this, 
            "/RMS_WarehouseOrder/Detail=/Order_Quantity/",
            i0);
    }

    /**
     * Path: /RMS_WarehouseOrder/Detail%/Retail_Price/TEXT,
     * example: "500".
     */
    public Path<String> retail_Price = 
        new Path<String>(this, "/RMS_WarehouseOrder/Detail%/Retail_Price/");

    /** Fetches fully indexed path, for DomUtil.set() and get(). */
    public Path<Float> retail_Price (int i0) {
        return new Path<Float>(this, 
            "/RMS_WarehouseOrder/Detail=/Retail_Price/",
            i0);
    }

    /**
     * Path: /RMS_WarehouseOrder/Detail%/TEXT.
     */
    public Path<String> detail_0 = 
        new Path<String>(this, "/RMS_WarehouseOrder/Detail%/");

    /** Fetches fully indexed path, for DomUtil.set() and get(). */
    public Path<String> detail_0 (int i0) {
        return new Path<String>(this, 
            "/RMS_WarehouseOrder/Detail=/",
            i0);
    }

    /**
     * Path: /RMS_WarehouseOrder/TEXT.
     */
    public Path<String> RMS_WarehouseOrder = 
        new Path<String>(this, "/RMS_WarehouseOrder/");

}
