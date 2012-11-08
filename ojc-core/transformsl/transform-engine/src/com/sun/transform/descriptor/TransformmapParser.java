/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)TransformmapParser.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.descriptor;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPathConstants;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.SAXParseException;
import com.sun.jbi.common.descriptor.CatalogResolver;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.parsers.AbstractJbiParser;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.transform.I18n;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.model.Param;
import com.sun.transform.engine.model.ProcessDef;
import com.sun.transform.engine.model.ProcessFactory;
import com.sun.transform.engine.model.Transform;
import com.sun.transform.engine.model.Param.Type;

/**
 * 
 * @author Kevan Simpson
 */
public class TransformmapParser extends AbstractJbiParser<ProcessDescriptor> {
    /** The default namespace of the transformmap configuration file. */
    public static final String TRANSFORMMAP_NS = 
            "http://www.sun.com/jbi/xsltse/transformmap";
    /** Static service name for XsltSE service endpoints. */
    public static final String XSLTSE_SERVICE_NAME = "xsltse";
    
    private static final String IMPORT_ELEM     = "import";
    private static final String SERVICE_ELEM    = "service";
    private static final String OPERATION_ELEM  = "operation";
    private static final String INVOKE_ELEM     = "invoke";
    private static final String TRANSFORM_ELEM  = "transform";
    private static final String PARAM_ELEM      = "param";
    
    private static final String NAMESPACE_ATTR          = "namespace";
    private static final String LOCATION_ATTR           = "location";
    private static final String TARGET_NS_ATTR          = "targetNamespace";
    private static final String PORT_TYPE_ATTR          = "portType";
    private static final String OPERATION_NAME_ATTR     = "opName";
    private static final String FILE_ATTR               = "file";
    private static final String SOURCE_ATTR             = "source";
    private static final String RESULT_ATTR             = "result";
    private static final String NAME_ATTR               = "name";
    private static final String TYPE_ATTR               = "type";
    private static final String VALUE_ATTR              = "value";
    private static final String INPUT_VAR_ATTR          = "inputVariable";
    private static final String OUTPUT_VAR_ATTR         = "outputVariable";
    private static final String VALIDATE_ATTR           = "validate";

    private Map<String, Definition> mWsdlMap;
    private ProcessDescriptor mDescriptor;
    private String mRootPath;
    private CatalogResolver mResolver;
    private ProcessFactory mFactory;

    /**
     * Constructs a parser with an installation root path and {@link ProcessFactory}.
     * 
     * @param rootPath The directory in which the Xslt service unit was installed.
     * @param fac A <code>ProcessFactory</code>.
     */
    public TransformmapParser(String rootPath, ProcessFactory fac) throws DeploymentException {
        mDescriptor = new ProcessDescriptor();
        mRootPath = rootPath;
        mResolver = CatalogResolver.newInstance(rootPath);
        mFactory = fac;
        mWsdlMap = new HashMap<String, Definition>();
        getNSContext().addNamespace("tmap", TRANSFORMMAP_NS);
    }

    /** @see com.sun.jbi.common.descriptor.parsers.JbiParser#parse(org.w3c.dom.Element) */
    public ProcessDescriptor parse(Element elem) throws DeploymentException {
        // expects the transformmap element passed in...
        try {
            setTargetNS(elem);
            // import wsdls
            NodeList imports = elem.getElementsByTagNameNS(
                    TRANSFORMMAP_NS, IMPORT_ELEM);
            if (imports != null) {
                for (int i = 0, n = imports.getLength(); i < n; i++) {
                    parseWSDL((Element) imports.item(i));
                }
            }
            // services
            NodeList srvcList = elem.getElementsByTagNameNS(
                    TRANSFORMMAP_NS, SERVICE_ELEM);
            if (srvcList != null) {
                for (int i = 0, n = srvcList.getLength(); i < n; i++) {
                    parseService((Element) srvcList.item(i));
                }
            }
        }
        catch (Exception e) {
            throw error(e, I18n.loc( 
                    "TRANSL-6001: Failed to parse transformmap descriptor: {0}", 
                    e.getMessage()));
        }
        
        return mDescriptor;
    }

    /**
     * Creates an {@link Invocation} from an &quot;invoke&quot; element.
     * @param elem The invoke element.
     * @param enclosingTransform The name of the enclosing transform activity,
     *                           if the invoke is defined as a transform child.
     * @return An <code>Invocation</code>.
     * @throws DeploymentException if an error occurs parsing content.
     */
    protected Invocation parseInvoke(Element elem, String enclosingTransform) 
            throws DeploymentException {
        PortType portType = findPortType(readQName(elem, PORT_TYPE_ATTR)); 
        String name = readNameAttr(elem);
        if (!Util.isEmpty(enclosingTransform)) {
            name = enclosingTransform +"."+ name;
        }
        
        EndpointInfo info = new EndpointInfo(false,   // consuming
                                             name,    // endpoint-name
                                             portType.getQName(), // interface-name
                                             getEndpointServiceName(), // service-name 
                                             null); // link-type
        Operation op = findOperation(portType, elem);
        // determine input/output variables
        String inputVar = elem.getAttribute(INPUT_VAR_ATTR);
        String outputVar = elem.getAttribute(OUTPUT_VAR_ATTR);
        if (Util.isEmpty(inputVar)) {
            inputVar = op.getInput().getMessage().getQName().getLocalPart();
        }
        if (Util.isEmpty(outputVar) && op.getOutput() != null) {
            outputVar = op.getOutput().getMessage().getQName().getLocalPart();
        }

        Invocation invoke = mFactory.createInvocation(
                name, info, op, inputVar, outputVar);
        NodeList faultTransformList = 
                elem.getElementsByTagNameNS(TRANSFORMMAP_NS, TRANSFORM_ELEM);
        if (faultTransformList != null) {
            for (int i = 0, n = faultTransformList.getLength(); i < n; i++) {
                Element transform = (Element) faultTransformList.item(i);
                Transform tr = parseTransform(transform, null);
                if (tr != null) {
                    invoke.addTransform(tr);
                }
            }
        }
        
        return invoke;
    }

    protected TransformEndpoint parseOperation(Element elem, 
                                               String srvcNm, 
                                               PortType portType) 
            throws DeploymentException {
        // expects operation element
        TransformEndpoint endpt = createEndpoint(elem, srvcNm, portType);
        // find operation
        Operation op = findOperation(portType, elem);
        if (endpt.getOperation(op.getName()) != null) {
            log().info(I18n.loc(
                    "TRANSL-5001: The process definition for operation {0} has been overwritten!", 
                    op.getName()));
        }

        ProcessDef def = createProcessDef(srvcNm, endpt.getInfo(), op,
                                          elem.getAttribute(INPUT_VAR_ATTR), 
                                          elem.getAttribute(OUTPUT_VAR_ATTR));
        endpt.setServiceDef(def, op);

        NodeList actList = elem.getElementsByTagNameNS(TRANSFORMMAP_NS, "*");
        if (actList != null) {
            for (int i = 0, n = actList.getLength(); i < n; i++) {
                Node node = actList.item(i);
                if (node instanceof Element) {
                    Element actElem = (Element) node;
                    if (actElem.getLocalName().equals(TRANSFORM_ELEM)) {
                        Transform tr = parseTransform(actElem, def);
                        def.addActivity(tr);
                        parseValidate(actElem, endpt, op);
                    }
                    else if (actElem.getLocalName().equals(INVOKE_ELEM)) {
                        Invocation invoke = parseInvoke(actElem, null);
                        // add to process def
                        def.addActivity(invoke);
                        // we register a dummy endpt for invocations later
                        TransformEndpoint consumed = 
                                new TransformEndpoint(invoke.getInfo());
                        mDescriptor.registerEndpoint(consumed);
                        parseValidate(actElem, endpt, invoke.getOperation());
                    }
                }
            }
        }
        
        return endpt;
    }
    
    protected void parseValidate(Element actElem, TransformEndpoint endpt, Operation op) {
        String validate = actElem.getAttribute(VALIDATE_ATTR);
        if (!Util.isEmpty(validate)) {
            
        }
    }
    
    /**
     * Creates a {@link Param} from an &quot;param&quot; element.
     * @param elem The param element.
     * @param tr The enclosing <code>Transform</code> activity.
     * @return A <code>Param</code>.
     * @throws DeploymentException if an error occurs parsing content.
     */
    protected Param parseParam(Element elem, Transform tr) throws DeploymentException {
        String rawType = elem.getAttribute(TYPE_ATTR);
        if (Util.isEmpty(rawType)) {
            throw error(null, I18n.loc(
                    "TRANSL-6006: Missing required param type for stylesheet: {0}", 
                    tr.getFile()));
        }
        
        Type type = Type.valueOf(rawType.toUpperCase());
        String val = strip(elem.getAttribute(VALUE_ATTR));
        Param p = mFactory.createParam(readNameAttr(elem), type);
        
        switch (type) {
            case URI: { // param value is content read from uri
                try {
                    Document doc = null;
                    File file = new File(mRootPath, val);
                    try {
                        doc = XmlUtil.readXml(file);
                    }
                    catch (SAXParseException spe) {
                        // assume the file contains only text, no xml
                        if (spe.getMessage().contains("Content is not allowed in prolog")) {
                            String content = Util.readFileContent(file);
                            p.setValue(content);
                            break;
                        }
                        else {
                            throw error(spe, I18n.loc(
                                    "TRANSL-6008: Failed to parse XML content in file \"{0}\" in {1}: {2}",
                                    String.valueOf(val), mRootPath, spe.getMessage()));
                        }
                    }
                    
                    Element root = doc.getDocumentElement();
                    // handle document fragments with dummy root "param"
                    if (root.getNodeName().equals("param")) {
                        p.setValue(XmlUtil.createDocumentFragment(
                                doc.getDocumentElement().getChildNodes()));
                    }
                    else {
                        p.setValue(root);
                    }
                }
                catch (FileNotFoundException fnfe) {
                    throw error(fnfe, I18n.loc(
                            "TRANSL-6007: Transform param content file \"{0}\" not found in {1}: {2}",  
                            String.valueOf(val), mRootPath, fnfe.getMessage()));
                }
                catch (DeploymentException de) {
                    throw de;
                }
                catch (Exception e) {
                    throw error(e, I18n.loc(
                            "TRANSL-6008: Failed to parse XML content in file \"{0}\" in {1}: {2}",
                            String.valueOf(val), mRootPath, e.getMessage()));
                }
                break;
            }
            case LITERAL: { // literal content in process definition
                try {
                    NodeList list = (NodeList) getXPath().evaluate("*", elem, XPathConstants.NODESET);
                    if (list == null || list.getLength() == 0) {
                        if (elem.getChildNodes().getLength() > 0) { // text node?
                            elem.normalize();
                            p.setValue(elem.getTextContent());
                        }
                        else {
                            p.setValue(val);
                        }
                    }
                    else if (list.getLength() == 1) {
                        Node child = list.item(0);
                        if (child instanceof Element) {
                            p.setValue((Element) child);
                        }
                        else if (child instanceof Text) {
                            p.setValue(child.getTextContent());
                        }
                    }
                    else {
                        DocumentFragment frag = XmlUtil.createDocumentFragment(list);
                        p.setValue(frag);
                    }
                }
                catch (Exception e) {
                    throw error(e, I18n.loc(
                            "TRANSL-6003: Failed to parse literal param content: {0}", e.getMessage()));
                }
                break;
            }
            default: {  // part name to pull param value at runtime
                p.setValue(val);
                break;
            }
        }
        
        // TODO log value at Finest
        return p; 
    }

    protected void parseService(Element elem) throws DeploymentException {
        String name = readNameAttr(elem);
        PortType portType = findPortType(readQName(elem, PORT_TYPE_ATTR));

        NodeList operList = elem.getElementsByTagNameNS(
                TRANSFORMMAP_NS, OPERATION_ELEM);
        if (operList != null) {
            for (int i = 0, n = operList.getLength(); i < n; i++) {
                Element opNode = (Element) operList.item(i);
                TransformEndpoint endpt = parseOperation(opNode, name, portType);
                mDescriptor.registerEndpoint(endpt);
            }
        }
    }

    /**
     * Creates a {@link Transform} activity from the specified element.
     * @param elem A DOM element, expected to be named &quot;transform&quot;.
     * @param procDef The transformation process definition.
     * @return A <code>Transform</code> activity def.
     * @throws DeploymentException if invalid content prevents creation.
     */
    protected Transform parseTransform(Element elem, ProcessDef procDef) 
            throws DeploymentException {
        String name = readNameAttr(elem),
               file = elem.getAttribute(FILE_ATTR),
               source = elem.getAttribute(SOURCE_ATTR), 
               result = elem.getAttribute(RESULT_ATTR);
        if (procDef == null) {
            if (Util.isEmpty(source) || Util.isEmpty(result)) {
                throw error(null, I18n.loc(
                        "TRANSL-6046: Must specify 'source' and 'result' for fault handler transform activities!"));
            }
        }
        else {
            Operation op = procDef.getInvocation().getOperation();
            
            // validate input part
            if (Util.isEmpty(source)) { // there can only be one part if none specified
                switch (op.getInput().getMessage().getParts().size()) {
                    case 0: {   // zero-part message
                        source = procDef//mCurrentEndpt.getServiceDef(op.getName())
                                    .getInvocation().getInputVariable();
                        break;
                    }
                    case 1: {   // if there's only 1 part, the attribute is optional
                        // we need to prepend the enclosing message's variable name
                        source = procDef//mCurrentEndpt.getServiceDef(op.getName())
                                        .getInvocation().getInputVariable() +
                                "."+ getPartName(op.getInput().getMessage(), 0);
                        break;
                    }
                    default: {
                        // allow for empty transforms, specifically: source only required if file is present
                        if (!Util.isEmpty(file)) {
                            throw error(null, I18n.loc(
                                    "TRANSL-6004: Must specify transformation input part for multi-part message in operation {0}", 
                                    op.getName()));
                        }
                    }
                }
            }
    
            // validate output part
            if (Util.isEmpty(result)) {
                String msgVar = procDef//mCurrentEndpt.getServiceDef(op.getName())
                                    .getInvocation().getOutputVariable();
                if (op.getOutput() != null) {
                    switch (op.getOutput().getMessage().getParts().size()) {
                        case 0: {   // zero-part message
                            result = (Util.isEmpty(msgVar) ? "" : msgVar);
                            break;
                        }
                        case 1: {
                            result = msgVar +"."+ 
                                    getPartName(op.getOutput().getMessage(), 0);
                            break;
                        }
                        default: {
                            // allow for empty transforms, specifically: source only required if file is present
                            if (!Util.isEmpty(file)) {
                                throw error(null, I18n.loc(
                                        "TRANSL-6005: Must specify transformation output part in operation {0}", 
                                        op.getName()));
                            }
                        }
                    }
                }
                else if (Util.isEmpty(msgVar)) {
                    result = "";
                }
                else {  // one-way op
                    result = msgVar +".status";
                }
            }
        }
        
        Transform tr = mFactory.createTransform(
                name, strip(file), strip(source), strip(result));
        // parse params
        NodeList parmList = elem.getElementsByTagNameNS(
                TRANSFORMMAP_NS, PARAM_ELEM);
        if (parmList != null) {
            for (int i = 0, n = parmList.getLength(); i < n; i++) {
                tr.addParam(parseParam((Element) parmList.item(i), tr));
            }
        }
        
        // parse extension invokes
        NodeList invokeList = elem.getElementsByTagNameNS(
                TRANSFORMMAP_NS, INVOKE_ELEM);
        if (invokeList != null) {
            for (int i = 0, n = invokeList.getLength(); i < n; i++) {
                tr.addInvocation(
                        parseInvoke((Element) invokeList.item(i), tr.getName()));
            }
        }

        return tr;
    }

    /**
     * Resolves an import statement in the transformmap.
     * @param elem The import element.
     * @throws DeploymentException if the import cannot be resolved.
     */
    protected void parseWSDL(Element elem) throws DeploymentException {
        String loc = elem.getAttribute(LOCATION_ATTR), 
               ns = elem.getAttribute(NAMESPACE_ATTR);
        try {
            if (Util.isEmpty(loc)) {
                throw error(null, I18n.loc(
                        "TRANSL-6016: {0} is a required attribute for Import element!",
                        LOCATION_ATTR));
            }
            else if (Util.isEmpty(ns)) {
                throw error(null, I18n.loc(
                        "TRANSL-6016: {0} is a required attribute for Import element!",
                        NAMESPACE_ATTR));
            }
            
            File f = new File(mRootPath, loc);
            if (!f.exists()) {
                f = mResolver.resolveFile(loc);
                if (f == null || !f.exists()) {
                    if (f == null) f = new File(mRootPath, loc);
                    throw error(null, I18n.loc(
                            "TRANSL-6009: Imported WSDL document does not exist at: {0}",
                            f.getAbsolutePath()));
                }
            }
            
            Definition def = XmlUtil.readWsdl(f, mResolver);
            String tns = def.getTargetNamespace();
            if (tns.equals(ns)) {
                // disallow duplicate wsdl imports
                if (mWsdlMap.containsKey(tns)) {
                    throw error(null, I18n.loc(
                            "TRANSL-6011: Imported WSDL namespace conflicts with previously imported WSDL: {0}",
                            tns));
                }
                
                // finally store the wsdl
                mWsdlMap.put(tns, def);
                if (log().isLoggable(Level.FINER)) {
                    log().finer("TRANSL-2001: WSDL[tns="+ tns +
                                "] loaded from: "+ f.getAbsolutePath());
                }
            }
            else {
                throw error(null, I18n.loc(
                        "TRANSL-6010: Imported WSDL namespace conflict, document namespace must match configured namespace!"));
            }
        }
        catch (DeploymentException de) {
            throw de;
        }
        catch (Exception e) {
            throw error(e, I18n.loc(
                    "TRANSL-6002: Failed to import WSDL({0}) from: {1}", 
                    ns, loc));
        }
    }
    /**
     * Creates a {@link TransformEndpoint} from an &quot;operation&quot; element.
     * @param elem The operation element.
     * @return a <code>TransformEndpoint</code>.
     * @throws DeploymentException if an error occurs parsing content.
     */
    protected TransformEndpoint createEndpoint(Element elem, String srvcNm, PortType portType) 
            throws DeploymentException {
        EndpointInfo info = 
                new EndpointInfo(true,
                                 srvcNm,
                                 portType.getQName(),
                                 getEndpointServiceName(),
                                 null); // no link-type specified for Xsltse
//        // find operation
//        Operation op = findOperation(portType, elem);
        TransformEndpoint endpt = mDescriptor.lookupEndpointDef(info);
        if (endpt == null) {
            endpt = new TransformEndpoint(info);
        }
//        else if (endpt.getOperation(op.getName()) != null) {
//            log().info(I18n.loc(
//                    "TRANSL-5001: The process definition for operation {0} has been overwritten!", 
//                    op.getName()));
//        }
//
//        ProcessDef def = createProcessDef(endpt.getInfo(), 
//                                          op,
//                                          elem.getAttribute(INPUT_VAR_ATTR), 
//                                          elem.getAttribute(OUTPUT_VAR_ATTR));
//        endpt.setServiceDef(def, op);
        return endpt;
    }
    
    /**
     * Creates a {@link ProcessDef} instance. 
     * @param info The endpoint info representing the implemented operation.
     * @param op The implemented operation.
     * @param inputVar The name of the incoming message, may be <code>null</code>.
     * @param outputVar The name of the outgoing message, may be <code>null</code>.
     * @return a <code>ProcessDef</code>.
     * @throws DeploymentException if invalid content prevents creation.
     */
    protected ProcessDef createProcessDef(String name, 
                                          EndpointInfo info, 
                                          Operation op,
                                          String inputVar, 
                                          String outputVar) throws DeploymentException {
        if (Util.isEmpty(inputVar)) {
            inputVar = op.getInput().getMessage().getQName().getLocalPart();
        }
        if (Util.isEmpty(outputVar) && op.getOutput() != null) {
            outputVar = op.getOutput().getMessage().getQName().getLocalPart();
        }
        
        return mFactory.createProcessDef(
                mFactory.createInvocation(name, info, op, inputVar, outputVar));
    }

    /**
     * Looks up a <code>PortType</code> by QName amongst imported WSDLs.
     * 
     * @param portTypeQName The qualified name of the port type to find.
     * @return a <code>PortType</code>.
     * @throws DeploymentException if the port type cannot be found.
     */
    protected PortType findPortType(QName portTypeQName) throws DeploymentException {
        if (portTypeQName == null) {
            throw error(null, I18n.loc(
                    "TRANSL-6013: Failed to locate WSDL with targetNamespace: {0}", 
                    "NULL"));
        }
        
        Definition wsdl = mWsdlMap.get(portTypeQName.getNamespaceURI());
        if (wsdl != null) {
            PortType portType = wsdl.getPortType(portTypeQName); 
            if (portType == null) {
                throw error(null, I18n.loc(
                        "TRANSL-6012: Undefined portType: {0}", 
                        String.valueOf(portTypeQName)));
            }
            
            return portType;
        }
        else {
            throw error(null, I18n.loc( 
                    "TRANSL-6013: Failed to locate WSDL with targetNamespace: {0}",  
                    portTypeQName.getNamespaceURI()));
        }
    }
    
    /**
     * Looks up an <code>Operation</code> amongst imported WSDLs.
     * 
     * @param portType The port type in which the operation is defined.
     * @param elem An operation element
     * @return an <code>Operation</code>.
     * @throws DeploymentException if the operation cannot be found.
     */
    protected Operation findOperation(PortType portType, 
                                      Element elem) throws DeploymentException {
        Operation operation = null;
        String opName = String.valueOf(elem.getAttribute(OPERATION_NAME_ATTR));
        try {
            operation = portType.getOperation(opName, null, null);
        }
        catch (IllegalArgumentException iae) {
            // multiple operations
            throw error(null, I18n.loc( 
                    "TRANSL-6014: Failed to distinguish between multiple operations \"{0}\" in portType: {1}", 
                    opName, String.valueOf(portType.getQName())));
        }
        
        if (operation == null) {
            throw error(null, I18n.loc(
                    "TRANSL-6015: Undefined operation \"{0}\" in portType: {1}",  
                    opName, String.valueOf(portType.getQName())));
        }
        
        return operation;
    }

    protected QName getEndpointServiceName() {
        return new QName(mDescriptor.getTargetNamespace(), XSLTSE_SERVICE_NAME);
    }

    /** Returns value of {@link #NAME_ATTR} attribute. */
    protected String readNameAttr(Element elem) throws DeploymentException {
        String name = elem.getAttribute(NAME_ATTR);
        if (Util.isEmpty(name)) {
            throw error(null, I18n.loc(
                        "TRANSL-6018: Transform descriptor is invalid - missing required 'name' attribute in element: {0}", 
                        elem.getLocalName()));
        }
        return name;
    }

    /** Resolves a QName from the specified attributes. */
    protected QName readQName(Element elem, String attrKey) throws DeploymentException {
        String qname = elem.getAttribute(attrKey);
        if (qname == null) {
            return null;
        }
        
        int index = qname.indexOf(":");
        if (index < 0) {
            throw error(null, I18n.loc(
                    "TRANSL-6019: Misconfigured Transform descriptor - unqualified QName: {0}", 
                    qname));
        }
        else {
            String prefix = qname.substring(0, index);
            // check for local namespace override
            String nsDecl = elem.lookupNamespaceURI(prefix);
            
            return new QName(nsDecl, qname.substring(index + 1), prefix);
        }
    }

    protected void setTargetNS(Element elem) throws DeploymentException {
        String targetNS = elem.getAttribute(TARGET_NS_ATTR);
        if (Util.isEmpty(targetNS)) {
            throw error(null, I18n.loc(
                    "TRANSL-6017: Transform descriptor is invalid - missing required 'targetNamespace' attribute!"));
        }
        else {
            mDescriptor.setTargetNamespace(targetNS);
        }
    }
    
    private String getPartName(Message msg, int index) {
        List parts = msg.getOrderedParts(null);
        return (index >= 0 && index < parts.size()) 
                ? ((Part) parts.get(index)).getName() : "unknownPart";
    }

    // strip the $ prefix from variable references and trim
    private String strip(String str) {
        if (str != null) {
            str = str.trim();
            return ((str.startsWith("$")) ? str.substring(1): str);
        }
        else return str;
    }
}
