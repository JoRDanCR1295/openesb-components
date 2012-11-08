//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2008.12.04 at 12:45:30 AM IST 
//


package com.sun.jbi.hl7bc.extservice.ack.hl7v26;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for OSP complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="OSP">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSP.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSP.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSP.3" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "OSP", propOrder = {
    "osp1",
    "osp2",
    "osp3"
})
public class OSP {

    @XmlElement(name = "OSP.1")
    protected OSP1CONTENT osp1;
    @XmlElement(name = "OSP.2")
    protected OSP2CONTENT osp2;
    @XmlElement(name = "OSP.3")
    protected OSP3CONTENT osp3;

    /**
     * Gets the value of the osp1 property.
     * 
     * @return
     *     possible object is
     *     {@link OSP1CONTENT }
     *     
     */
    public OSP1CONTENT getOSP1() {
        return osp1;
    }

    /**
     * Sets the value of the osp1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSP1CONTENT }
     *     
     */
    public void setOSP1(OSP1CONTENT value) {
        this.osp1 = value;
    }

    /**
     * Gets the value of the osp2 property.
     * 
     * @return
     *     possible object is
     *     {@link OSP2CONTENT }
     *     
     */
    public OSP2CONTENT getOSP2() {
        return osp2;
    }

    /**
     * Sets the value of the osp2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSP2CONTENT }
     *     
     */
    public void setOSP2(OSP2CONTENT value) {
        this.osp2 = value;
    }

    /**
     * Gets the value of the osp3 property.
     * 
     * @return
     *     possible object is
     *     {@link OSP3CONTENT }
     *     
     */
    public OSP3CONTENT getOSP3() {
        return osp3;
    }

    /**
     * Sets the value of the osp3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSP3CONTENT }
     *     
     */
    public void setOSP3(OSP3CONTENT value) {
        this.osp3 = value;
    }

}