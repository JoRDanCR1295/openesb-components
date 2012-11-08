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
 * <p>Java class for PPN complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PPN">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.4" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.5" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.6" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.7" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.8" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.9" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.10" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.11" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.12" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.13" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.14" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.15" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.16" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.17" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.18" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.19" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.20" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.21" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.22" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.23" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PPN.24" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PPN", propOrder = {
    "ppn1",
    "ppn2",
    "ppn3",
    "ppn4",
    "ppn5",
    "ppn6",
    "ppn7",
    "ppn8",
    "ppn9",
    "ppn10",
    "ppn11",
    "ppn12",
    "ppn13",
    "ppn14",
    "ppn15",
    "ppn16",
    "ppn17",
    "ppn18",
    "ppn19",
    "ppn20",
    "ppn21",
    "ppn22",
    "ppn23",
    "ppn24"
})
public class PPN {

    @XmlElement(name = "PPN.1")
    protected PPN1CONTENT ppn1;
    @XmlElement(name = "PPN.2")
    protected PPN2CONTENT ppn2;
    @XmlElement(name = "PPN.3")
    protected PPN3CONTENT ppn3;
    @XmlElement(name = "PPN.4")
    protected PPN4CONTENT ppn4;
    @XmlElement(name = "PPN.5")
    protected PPN5CONTENT ppn5;
    @XmlElement(name = "PPN.6")
    protected PPN6CONTENT ppn6;
    @XmlElement(name = "PPN.7")
    protected PPN7CONTENT ppn7;
    @XmlElement(name = "PPN.8")
    protected PPN8CONTENT ppn8;
    @XmlElement(name = "PPN.9")
    protected PPN9CONTENT ppn9;
    @XmlElement(name = "PPN.10")
    protected PPN10CONTENT ppn10;
    @XmlElement(name = "PPN.11")
    protected PPN11CONTENT ppn11;
    @XmlElement(name = "PPN.12")
    protected PPN12CONTENT ppn12;
    @XmlElement(name = "PPN.13")
    protected PPN13CONTENT ppn13;
    @XmlElement(name = "PPN.14")
    protected PPN14CONTENT ppn14;
    @XmlElement(name = "PPN.15")
    protected PPN15CONTENT ppn15;
    @XmlElement(name = "PPN.16")
    protected PPN16CONTENT ppn16;
    @XmlElement(name = "PPN.17")
    protected PPN17CONTENT ppn17;
    @XmlElement(name = "PPN.18")
    protected PPN18CONTENT ppn18;
    @XmlElement(name = "PPN.19")
    protected PPN19CONTENT ppn19;
    @XmlElement(name = "PPN.20")
    protected PPN20CONTENT ppn20;
    @XmlElement(name = "PPN.21")
    protected PPN21CONTENT ppn21;
    @XmlElement(name = "PPN.22")
    protected PPN22CONTENT ppn22;
    @XmlElement(name = "PPN.23")
    protected PPN23CONTENT ppn23;
    @XmlElement(name = "PPN.24")
    protected PPN24CONTENT ppn24;

    /**
     * Gets the value of the ppn1 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN1CONTENT }
     *     
     */
    public PPN1CONTENT getPPN1() {
        return ppn1;
    }

    /**
     * Sets the value of the ppn1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN1CONTENT }
     *     
     */
    public void setPPN1(PPN1CONTENT value) {
        this.ppn1 = value;
    }

    /**
     * Gets the value of the ppn2 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN2CONTENT }
     *     
     */
    public PPN2CONTENT getPPN2() {
        return ppn2;
    }

    /**
     * Sets the value of the ppn2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN2CONTENT }
     *     
     */
    public void setPPN2(PPN2CONTENT value) {
        this.ppn2 = value;
    }

    /**
     * Gets the value of the ppn3 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN3CONTENT }
     *     
     */
    public PPN3CONTENT getPPN3() {
        return ppn3;
    }

    /**
     * Sets the value of the ppn3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN3CONTENT }
     *     
     */
    public void setPPN3(PPN3CONTENT value) {
        this.ppn3 = value;
    }

    /**
     * Gets the value of the ppn4 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN4CONTENT }
     *     
     */
    public PPN4CONTENT getPPN4() {
        return ppn4;
    }

    /**
     * Sets the value of the ppn4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN4CONTENT }
     *     
     */
    public void setPPN4(PPN4CONTENT value) {
        this.ppn4 = value;
    }

    /**
     * Gets the value of the ppn5 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN5CONTENT }
     *     
     */
    public PPN5CONTENT getPPN5() {
        return ppn5;
    }

    /**
     * Sets the value of the ppn5 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN5CONTENT }
     *     
     */
    public void setPPN5(PPN5CONTENT value) {
        this.ppn5 = value;
    }

    /**
     * Gets the value of the ppn6 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN6CONTENT }
     *     
     */
    public PPN6CONTENT getPPN6() {
        return ppn6;
    }

    /**
     * Sets the value of the ppn6 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN6CONTENT }
     *     
     */
    public void setPPN6(PPN6CONTENT value) {
        this.ppn6 = value;
    }

    /**
     * Gets the value of the ppn7 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN7CONTENT }
     *     
     */
    public PPN7CONTENT getPPN7() {
        return ppn7;
    }

    /**
     * Sets the value of the ppn7 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN7CONTENT }
     *     
     */
    public void setPPN7(PPN7CONTENT value) {
        this.ppn7 = value;
    }

    /**
     * Gets the value of the ppn8 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN8CONTENT }
     *     
     */
    public PPN8CONTENT getPPN8() {
        return ppn8;
    }

    /**
     * Sets the value of the ppn8 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN8CONTENT }
     *     
     */
    public void setPPN8(PPN8CONTENT value) {
        this.ppn8 = value;
    }

    /**
     * Gets the value of the ppn9 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN9CONTENT }
     *     
     */
    public PPN9CONTENT getPPN9() {
        return ppn9;
    }

    /**
     * Sets the value of the ppn9 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN9CONTENT }
     *     
     */
    public void setPPN9(PPN9CONTENT value) {
        this.ppn9 = value;
    }

    /**
     * Gets the value of the ppn10 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN10CONTENT }
     *     
     */
    public PPN10CONTENT getPPN10() {
        return ppn10;
    }

    /**
     * Sets the value of the ppn10 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN10CONTENT }
     *     
     */
    public void setPPN10(PPN10CONTENT value) {
        this.ppn10 = value;
    }

    /**
     * Gets the value of the ppn11 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN11CONTENT }
     *     
     */
    public PPN11CONTENT getPPN11() {
        return ppn11;
    }

    /**
     * Sets the value of the ppn11 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN11CONTENT }
     *     
     */
    public void setPPN11(PPN11CONTENT value) {
        this.ppn11 = value;
    }

    /**
     * Gets the value of the ppn12 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN12CONTENT }
     *     
     */
    public PPN12CONTENT getPPN12() {
        return ppn12;
    }

    /**
     * Sets the value of the ppn12 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN12CONTENT }
     *     
     */
    public void setPPN12(PPN12CONTENT value) {
        this.ppn12 = value;
    }

    /**
     * Gets the value of the ppn13 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN13CONTENT }
     *     
     */
    public PPN13CONTENT getPPN13() {
        return ppn13;
    }

    /**
     * Sets the value of the ppn13 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN13CONTENT }
     *     
     */
    public void setPPN13(PPN13CONTENT value) {
        this.ppn13 = value;
    }

    /**
     * Gets the value of the ppn14 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN14CONTENT }
     *     
     */
    public PPN14CONTENT getPPN14() {
        return ppn14;
    }

    /**
     * Sets the value of the ppn14 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN14CONTENT }
     *     
     */
    public void setPPN14(PPN14CONTENT value) {
        this.ppn14 = value;
    }

    /**
     * Gets the value of the ppn15 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN15CONTENT }
     *     
     */
    public PPN15CONTENT getPPN15() {
        return ppn15;
    }

    /**
     * Sets the value of the ppn15 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN15CONTENT }
     *     
     */
    public void setPPN15(PPN15CONTENT value) {
        this.ppn15 = value;
    }

    /**
     * Gets the value of the ppn16 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN16CONTENT }
     *     
     */
    public PPN16CONTENT getPPN16() {
        return ppn16;
    }

    /**
     * Sets the value of the ppn16 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN16CONTENT }
     *     
     */
    public void setPPN16(PPN16CONTENT value) {
        this.ppn16 = value;
    }

    /**
     * Gets the value of the ppn17 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN17CONTENT }
     *     
     */
    public PPN17CONTENT getPPN17() {
        return ppn17;
    }

    /**
     * Sets the value of the ppn17 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN17CONTENT }
     *     
     */
    public void setPPN17(PPN17CONTENT value) {
        this.ppn17 = value;
    }

    /**
     * Gets the value of the ppn18 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN18CONTENT }
     *     
     */
    public PPN18CONTENT getPPN18() {
        return ppn18;
    }

    /**
     * Sets the value of the ppn18 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN18CONTENT }
     *     
     */
    public void setPPN18(PPN18CONTENT value) {
        this.ppn18 = value;
    }

    /**
     * Gets the value of the ppn19 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN19CONTENT }
     *     
     */
    public PPN19CONTENT getPPN19() {
        return ppn19;
    }

    /**
     * Sets the value of the ppn19 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN19CONTENT }
     *     
     */
    public void setPPN19(PPN19CONTENT value) {
        this.ppn19 = value;
    }

    /**
     * Gets the value of the ppn20 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN20CONTENT }
     *     
     */
    public PPN20CONTENT getPPN20() {
        return ppn20;
    }

    /**
     * Sets the value of the ppn20 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN20CONTENT }
     *     
     */
    public void setPPN20(PPN20CONTENT value) {
        this.ppn20 = value;
    }

    /**
     * Gets the value of the ppn21 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN21CONTENT }
     *     
     */
    public PPN21CONTENT getPPN21() {
        return ppn21;
    }

    /**
     * Sets the value of the ppn21 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN21CONTENT }
     *     
     */
    public void setPPN21(PPN21CONTENT value) {
        this.ppn21 = value;
    }

    /**
     * Gets the value of the ppn22 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN22CONTENT }
     *     
     */
    public PPN22CONTENT getPPN22() {
        return ppn22;
    }

    /**
     * Sets the value of the ppn22 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN22CONTENT }
     *     
     */
    public void setPPN22(PPN22CONTENT value) {
        this.ppn22 = value;
    }

    /**
     * Gets the value of the ppn23 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN23CONTENT }
     *     
     */
    public PPN23CONTENT getPPN23() {
        return ppn23;
    }

    /**
     * Sets the value of the ppn23 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN23CONTENT }
     *     
     */
    public void setPPN23(PPN23CONTENT value) {
        this.ppn23 = value;
    }

    /**
     * Gets the value of the ppn24 property.
     * 
     * @return
     *     possible object is
     *     {@link PPN24CONTENT }
     *     
     */
    public PPN24CONTENT getPPN24() {
        return ppn24;
    }

    /**
     * Sets the value of the ppn24 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PPN24CONTENT }
     *     
     */
    public void setPPN24(PPN24CONTENT value) {
        this.ppn24 = value;
    }

}
