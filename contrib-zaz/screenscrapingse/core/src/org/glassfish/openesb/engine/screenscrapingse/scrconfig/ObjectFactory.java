//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vJAXB 2.0 in JDK 1.6 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.05.10 at 01:36:48 PM IST 
//
package org.glassfish.openesb.engine.screenscrapingse.scrconfig;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;

/**
 * This object contains factory methods for each
 * Java content interface and Java element interface
 * generated in the com.sun.jbi.engine.scriptse.scrconfig package.
 * <p>An ObjectFactory allows you to programatically
 * construct new instances of the Java representation
 * for XML content. The Java representation of XML
 * content can consist of schema derived interfaces
 * and classes representing the binding of schema
 * type definitions, element declarations and model
 * groups.  Factory methods for each of these are
 * provided in this class.
 *
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _Extension_QNAME = new QName("", "extension");
    private final static QName _Mimetype_QNAME = new QName("", "mimetype");
    private final static QName _Enginename_QNAME = new QName("", "enginename");
    private final static QName _Filename_QNAME = new QName("", "filename");
    private final static QName _Mode_QNAME = new QName("", "mode");
    private final static QName _Version_QNAME = new QName("", "version");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.sun.jbi.engine.scriptse.scrconfig
     *
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Input }
     *
     */
    public Input createInput() {
        return new Input();
    }

    /**
     * Create an instance of {@link Operation }
     *
     */
    public Operation createOperation() {
        return new Operation();
    }

    /**
     * Create an instance of {@link RequestReplyService }
     *
     */
    public RequestReplyService createRequestReplyService() {
        return new RequestReplyService();
    }

    /**
     * Create an instance of {@link Scriptconfig }
     *
     */
    public Scriptconfig createScriptconfig() {
        return new Scriptconfig();
    }

    /**
     * Create an instance of {@link Parameter }
     *
     */
    public Parameter createParameter() {
        return new Parameter();
    }

    /**
     * Create an instance of {@link Scriptconfig.ScrEngineDetails }
     *
     */
    public Scriptconfig.ScrEngineDetails createScriptconfigScrEngineDetails() {
        return new Scriptconfig.ScrEngineDetails();
    }

    /**
     * Create an instance of {@link Parameter }
     *
     */
    public FilterOneWay createFilterOneWay() {
        return new FilterOneWay();
    }

    /**
     * Create an instance of {@link Output }
     *
     */
    public Operations createOperations() {
        return new Operations();
    }

    /**
     * Create an instance of {@link Output }
     *
     */
    public Output createOutput() {
        return new Output();
    }

    /**
     * Create an instance of {@link Returntype }
     *
     */
    public Returntype createReturntype() {
        return new Returntype();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     *
     */
    @XmlElementDecl(namespace = "", name = "extension")
    public JAXBElement<String> createExtension(String value) {
        return new JAXBElement<String>(_Extension_QNAME, String.class, null,
                value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     *
     */
    @XmlElementDecl(namespace = "", name = "mimetype")
    public JAXBElement<String> createMimetype(String value) {
        return new JAXBElement<String>(_Mimetype_QNAME, String.class, null,
                value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     *
     */
    @XmlElementDecl(namespace = "", name = "enginename")
    public JAXBElement<String> createEnginename(String value) {
        return new JAXBElement<String>(_Enginename_QNAME, String.class, null,
                value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     *
     */
    @XmlElementDecl(namespace = "", name = "filename")
    public JAXBElement<String> createFilename(String value) {
        return new JAXBElement<String>(_Filename_QNAME, String.class, null,
                value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     *
     */
    @XmlElementDecl(namespace = "", name = "mode")
    public JAXBElement<String> createMode(String value) {
        return new JAXBElement<String>(_Mode_QNAME, String.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     *
     */
    @XmlElementDecl(namespace = "", name = "version")
    public JAXBElement<String> createVersion(String value) {
        return new JAXBElement<String>(_Version_QNAME, String.class, null, value);
    }
}
