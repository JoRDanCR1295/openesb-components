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
 * <p>Java class for CD complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CD">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}CD.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CD.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CD.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CD.4" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CD.5" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CD.6" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CD", propOrder = {
    "cd1",
    "cd2",
    "cd3",
    "cd4",
    "cd5",
    "cd6"
})
public class CD {

    @XmlElement(name = "CD.1")
    protected CD1CONTENT cd1;
    @XmlElement(name = "CD.2")
    protected CD2CONTENT cd2;
    @XmlElement(name = "CD.3")
    protected CD3CONTENT cd3;
    @XmlElement(name = "CD.4")
    protected CD4CONTENT cd4;
    @XmlElement(name = "CD.5")
    protected CD5CONTENT cd5;
    @XmlElement(name = "CD.6")
    protected CD6CONTENT cd6;

    /**
     * Gets the value of the cd1 property.
     * 
     * @return
     *     possible object is
     *     {@link CD1CONTENT }
     *     
     */
    public CD1CONTENT getCD1() {
        return cd1;
    }

    /**
     * Sets the value of the cd1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CD1CONTENT }
     *     
     */
    public void setCD1(CD1CONTENT value) {
        this.cd1 = value;
    }

    /**
     * Gets the value of the cd2 property.
     * 
     * @return
     *     possible object is
     *     {@link CD2CONTENT }
     *     
     */
    public CD2CONTENT getCD2() {
        return cd2;
    }

    /**
     * Sets the value of the cd2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CD2CONTENT }
     *     
     */
    public void setCD2(CD2CONTENT value) {
        this.cd2 = value;
    }

    /**
     * Gets the value of the cd3 property.
     * 
     * @return
     *     possible object is
     *     {@link CD3CONTENT }
     *     
     */
    public CD3CONTENT getCD3() {
        return cd3;
    }

    /**
     * Sets the value of the cd3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CD3CONTENT }
     *     
     */
    public void setCD3(CD3CONTENT value) {
        this.cd3 = value;
    }

    /**
     * Gets the value of the cd4 property.
     * 
     * @return
     *     possible object is
     *     {@link CD4CONTENT }
     *     
     */
    public CD4CONTENT getCD4() {
        return cd4;
    }

    /**
     * Sets the value of the cd4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CD4CONTENT }
     *     
     */
    public void setCD4(CD4CONTENT value) {
        this.cd4 = value;
    }

    /**
     * Gets the value of the cd5 property.
     * 
     * @return
     *     possible object is
     *     {@link CD5CONTENT }
     *     
     */
    public CD5CONTENT getCD5() {
        return cd5;
    }

    /**
     * Sets the value of the cd5 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CD5CONTENT }
     *     
     */
    public void setCD5(CD5CONTENT value) {
        this.cd5 = value;
    }

    /**
     * Gets the value of the cd6 property.
     * 
     * @return
     *     possible object is
     *     {@link CD6CONTENT }
     *     
     */
    public CD6CONTENT getCD6() {
        return cd6;
    }

    /**
     * Sets the value of the cd6 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CD6CONTENT }
     *     
     */
    public void setCD6(CD6CONTENT value) {
        this.cd6 = value;
    }

}