/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator.bcm;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.webservice.generator.AnyType;
import it.imolinfo.jbi4corba.webservice.generator.InterfaceType;
import it.imolinfo.jbi4corba.webservice.generator.MethodSignature;
import it.imolinfo.jbi4corba.webservice.generator.Param;
import it.imolinfo.jbi4corba.webservice.generator.TypeUtils;
import it.imolinfo.jbi4corba.webservice.generator.UnionType;
import it.imolinfo.jbi4corba.webservice.generator.UnionTypeUtils;
import it.imolinfo.jbi4corba.webservice.generator.Util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Type;

/**
 * This class is used to add the annotations for the WebService. 
 * Also collect the exceptions classes name thrown by the operation methods.
 *
 * FIXME with method overloading the behavior is undefined.
 */
public class WebServiceAnnotationAdapter extends ClassAdapter {

    /**
     * A JSR181 annotation internal class name.
     * <pre>
     * ==========================================================================
     * member            | mean                              | default
     * ==========================================================================
     * name              | the name of the WS.               | Simple name of the
     *                   | Used as the name of               | java class or
     *                   | the wsdl:portType                 | interface.
     * --------------------------------------------------------------------------
     * targetNamespace   | used for wsdl:portType            | see JaxWs 2.0
     *                   | or wsdl:service or both           |
     * --------------------------------------------------------------------------
     * serviceName       | wsdl:service (not allowed         | simple name of the
     *                   | on endpoint interface)            | java class
     *                   |                                   | + Service
     * --------------------------------------------------------------------------
     * portName          | wsdl:port (not allowed on         | WS name + Port
     *                   | endpoint interface)               |
     * --------------------------------------------------------------------------
     * wsdlLocation      |                                   | none
     * --------------------------------------------------------------------------
     * endpointInterface | (not allowed on                   | none
     *                   | endpoint interface)               |
     * ==========================================================================
     * </pre>
     */
    public static final String JSR181_WEB_SERVICE = "Ljavax/jws/WebService;";
    private static final String W3CEPR = Type.getDescriptor(javax.xml.ws.wsaddressing.W3CEndpointReference.class);
    /**
     * A JSR181 annotation internal class name.
     * <pre>
     * ==========================================================================
     * member            | mean                              | default
     * ==========================================================================
     * operationName     | wsdl:operation name               | name of the java
     *                   |                                   | method
     * --------------------------------------------------------------------------
     * action            | the soap action                   | ""
     * --------------------------------------------------------------------------
     * exclude           | a method not exposed (not allowed | false
     *                   | on endpoint interface)            |
     * ==========================================================================
     * </pre>
     */
    public static final String JSR181_WEB_METHOD = "Ljavax/jws/WebMethod;";
    /**
     * <pre>
     * ==========================================================================
     * member            | mean                              | default
     * ==========================================================================
     * name              | name of the parameter.            | WebMethod.operation
     *                   |                                   | name if the
     *                   | If RPC and WebParam.partName has  | operation is
     *                   | not been specified, this is the   | DOCUMENT style and
     *                   | name of the wsdl:part             | the parameter style
     *                   |                                   | is BARE.
     *                   | If DOCUMENT or the parameter maps |
     *                   | to a header, this is the local    | Otherwise:
     *                   | name of the XML element           | arg0,arg1,...
     *                   | representing the parameter.       |
     *                   |                                   |
     *                   | A name MUST be specified if the   |
     *                   | operation is DOCUMENT style, the  |
     *                   | parameter style is BARE, and the  |
     *                   | mode is OUT or INOUT.             |
     * --------------------------------------------------------------------------
     * partName          | the wsdl:part name.               | WebParam.name
     *                   | Used only if the operation is RPC |
     *                   | or the operation is DOCUMENT and  |
     *                   | parameter style is BARE           |
     * --------------------------------------------------------------------------
     * mode              | the direction of the parameter    | INOUT if Holder
     *                   | on endpoint interface)            | type otherwise IN
     *                   |                                   | (see JaxWs 2.0)
     * --------------------------------------------------------------------------
     * header            | If true the parameter is pulled   | false
     *                   | from a message header rather then |
     *                   | the message body                  |
     * ==========================================================================
     * </pre>
     */
    public static final String JSR181_WEB_PARAM = "Ljavax/jws/WebParam;";
    public static final String JSR181_WEB_PARAM_MODE = "Ljavax/jws/WebParam$Mode;";
    public static final String JSR181_WEB_PARAM_MODE_INOUT = "INOUT";
    public static final String JSR181_WEB_RESULT = "Ljavax/jws/WebResult;";
    public static final String JAVAX_XML_WS_HOLDER_TYPE = "Ljavax/xml/ws/Holder;";
    public static final String JAVAX_XML_WS_HOLDER_TYPE_CANONICAL_NAME = "javax.xml.ws.Holder";
    public static final String JAVAX_XML_WS_HOLDER_TYPE_NO_SEMICOLON = "Ljavax/xml/ws/Holder";
    public static final String JSR181_SOAP_BINDING = "Ljavax/jws/soap/SOAPBinding;";
    public static final String JAXB_XML_SEE_ALSO_ANNOTATOIN = "Ljavax/xml/bind/annotation/XmlSeeAlso;";
    public static final String JSR181_SOAP_BINDING_USE = "Ljavax/jws/soap/SOAPBinding$Use;";
    public static final String JSR181_SOAP_BINDING_STYLE = "Ljavax/jws/soap/SOAPBinding$ParameterStyle;";
    public static final String JSR181_LITERAL = "LITERAL";
    public static final String JSR181_WRAPPED = "WRAPPED";
    public static final String JSR181_BARE = "BARE";
    private Map<String, UnionType> allUnionTypes;
    private Map<String, InterfaceType> allInterfaceTypes;
    private String workingDirClasses;
    /* Exceptions list */
    public Set<String> exceptionsThrown = new HashSet<String>();
    /**
     * Used to mark an asynchronous WebMethod.
     */
    public static final String JSR181_ONEWAY = "Ljavax/jws/Oneway;";
    protected static Set<String> noAnnotatedMethods = new HashSet<String>();


    static {
        noAnnotatedMethods.add("<init>");
        noAnnotatedMethods.add("hashCode");
        noAnnotatedMethods.add("getClass");
        noAnnotatedMethods.add("wait");
        noAnnotatedMethods.add("equals");
        noAnnotatedMethods.add("notify");
        noAnnotatedMethods.add("notifyAll");
        noAnnotatedMethods.add("toString");
    }
    /**
     * Logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(WebServiceAnnotationAdapter.class);
    /** The class writer for this bytecode manipulation. */
    protected ClassWriter classWriter = null;
    /** The list of method's signature. */
    protected List<MethodSignature> methodSignatureList = null;
    boolean areClassAnnotationsPresent = false;
    /** The namespace */
    protected String nameSpace = null;
    /** The porttype  wsdl name */
    protected String portTypeName = null;
    /** All types  in idl */
    protected Set<Class> allTypes = null;

    /**
     * Constructor.
     * 
     * @param cv The class visitor.
     * @param cw The class writer.
     * @param aMethodSignatureList The list of method to manipulate.
     * @param ns
     * @param portTypeName if not null, the port type name
     */
    public WebServiceAnnotationAdapter(ClassVisitor cv, ClassWriter cw,
            List<MethodSignature> aMethodSignatureList, String ns, String ptn, String workingDir, Map<String, UnionType> unionTypes, Map<String, InterfaceType> intfTypes, Set<Class> allTypesIDL) {

        super(cv);

        classWriter = cw;
        methodSignatureList = aMethodSignatureList;
        nameSpace = ns;
        portTypeName = ptn;
        allUnionTypes = unionTypes;
        allInterfaceTypes = intfTypes;
        workingDirClasses = workingDir;
        allTypes = allTypesIDL;
    }

    /**
     * Override.
     * @param version     The version
     * @param access      The access
     * @param name        The name
     * @param signature   The signature
     * @param superName   The super name
     * @param interfaces  The interfaces
     */
    @Override
    public void visit(int version, int access, String name, String signature,
            String superName, String[] interfaces) {

        LOG.debug("CRB000605_WebServiceAnnotationAdapter_visit", new Object[]{version, access, name, signature, superName, interfaces});

        super.visit(version, access, name, signature, superName, interfaces);

        addAnnotation(name);

    }

    @Override
    public void visitEnd() {

        cv.visitEnd();
    }

    /**
     * Adds the Webservice and SOAPBinding annotations
     * @param name The service name
     */
    private void addAnnotation(String name) {
        if (!areClassAnnotationsPresent) {

            // Adds the WebService annotation
            AnnotationVisitor av = cv.visitAnnotation(JSR181_WEB_SERVICE, true);

            // If the port type is pspecified, uses it. Otherwise uses the java class name.
            if (portTypeName != null) {
                LOG.debug("Adding port type name:" + portTypeName);
                av.visit("name", portTypeName);
            } else {
                String javaSimpleName = name.substring(name.lastIndexOf("/") + 1);
                av.visit("name", javaSimpleName);
            }
            if (nameSpace != null) {
                av.visit("targetNamespace", nameSpace);
                LOG.debug("Adding targetNamespace:" + nameSpace);
            }
            if (av != null) {
                av.visitEnd();
            }

            // Adds the SOAP annotation
            AnnotationVisitor av2 = cv.visitAnnotation(JSR181_SOAP_BINDING, true);
            av2.visitEnum("use", JSR181_SOAP_BINDING_USE, JSR181_LITERAL);
            av2.visitEnum("parameterStyle",
                    JSR181_SOAP_BINDING_STYLE, JSR181_WRAPPED);


            if (av2 != null) {
                av2.visitEnd();
            }

            // Adds the XMLSeeAlso annotation
            AnnotationVisitor av3 = cv.visitAnnotation(JAXB_XML_SEE_ALSO_ANNOTATOIN, true);
            AnnotationVisitor xmlElems = av3.visitArray("value");

            for (Class typeClass : allTypes) {
                String className = typeClass.getName();
                UnionType union = UnionTypeUtils.isUnionType(className, false, allUnionTypes);
                if (union != null) {
                    try {
                        typeClass = Util.classLoad(workingDirClasses, union.getTypeName() + "Wrapper");
                    } catch (ClassGenerationException e) {
                        //class not found ignoring
                        LOG.debug("WebServiceAnnotationAdapter.addAnnotation: Class not found for union: " + union.getTypeName());
                    }
                }

                if (!Util.isThrowableSubClass(typeClass) && !typeClass.isInterface()) {

                    xmlElems.visit("value", Type.getType(typeClass));
                    //Raf: added to manage jbi4corba-7 issue from nokia. To be removed if typedef defined type management is added
                    //all idlj generated java primitive types are always added in the array version
                    if (!typeClass.isArray() && !typeClass.isAnonymousClass() && !typeClass.isPrimitive()) {
                        if (LOG.isDebugEnabled()){
                        LOG.debug("orginal class: " + typeClass);
                        LOG.debug("type descriptor: " + Type.getType(String.class).getDescriptor());
                        LOG.debug("typeArray descriptor: " + Type.getType(String[].class).getDescriptor());
                        LOG.debug("annotation for arrays: " + Type.getType("[" + Type.getType(typeClass).getDescriptor()));
                        }
                        //LOG.debug("annotation for arrays2: "+Type.getObjectType("["+typeClass.getCanonicalName()));
                        xmlElems.visit("value", Type.getType("[" + Type.getType(typeClass).getDescriptor()));
                    }
                }
            }

            //Raf: added to manage jbi4corba-7 issue from nokia. To be removed if typedef defined type management is added
            //all idlj generated java primitive types are always added in the array version            
            /*
            xmlElems.visit("value", Type.getType(boolean[].class));
            xmlElems.visit("value", Type.getType(char[].class));
            xmlElems.visit("value", Type.getType(byte[].class));
            xmlElems.visit("value", Type.getType(String[].class));
            xmlElems.visit("value", Type.getType(String[][].class));
            xmlElems.visit("value", Type.getType(short[].class));
            xmlElems.visit("value", Type.getType(int[].class));
            xmlElems.visit("value", Type.getType(long[].class));
            xmlElems.visit("value", Type.getType(float[].class));
            xmlElems.visit("value", Type.getType(double[].class));
            xmlElems.visit("value", Type.getType(java.math.BigDecimal[].class));
             */            

            if (xmlElems != null) {
                xmlElems.visitEnd();
            }
            if (av3 != null) {
                av3.visitEnd();
            }

            areClassAnnotationsPresent = true;
        }
    }

    /**
     * Override.
     * @param access      The access
     * @param name        The name
     * @param desc        The desc
     * @param signature   The signature
     * @param exceptions  The exceptions
     * @return            The return
     */
    @Override
    public MethodVisitor visitMethod(int access, String name, String desc,
            String signature, String[] exceptions) {

        LOG.debug("WebServiceAnnotationAdapter.visitMethod. access=" + access + "; name=" + name + "; desc=" + desc + "; signature=" + signature + "; exceptions=" + exceptions);

        if (exceptions != null) {
            for (int i = 0; i < exceptions.length; i++) {
                LOG.debug("Exception found: " + exceptions[i]);
                exceptionsThrown.add(exceptions[i]);
            }
        }

        if (noAnnotatedMethods.contains(name)) {
            LOG.debug("NO WebService Annotations [methodName=" + name + "]");
            return super.visitMethod(access, name, desc, signature, exceptions);
        }

        LOG.debug("WebService Annotations for [methodName=" + name + "]");

        // Gets the method visitor
        MethodSignature methodSignature = findFirstMethodSignature(name);

        // Change the Holder parameter (if holder are present in the parameters.
        //**********************************************************************
        // INOUT

        if (methodSignature.isContainsHolder()) {
            SignatureDescription sd = changeHolderParameters(methodSignature, desc, signature);
            desc = sd.getDescription();
            signature = sd.getSignature();
        }
        //**********************************************************************


        MethodVisitor mv = super.visitMethod(access, name, desc, signature, exceptions);

        addMethodSignatureAnnotation(mv, methodSignature, desc, signature);

        return mv;

    }

    /**
     * Add annotation: WebMethod, Oneway, WebResult, WebParam.
     * 
     * @param methodVisitor The method visitor
     * @param sig the Method Signature
     * @param methodDescription the bytecode method description
     */
    protected void addMethodSignatureAnnotation(MethodVisitor methodVisitor,
            MethodSignature methodSignature, String description, String signature) {

        addAnnotationOperationName(methodVisitor, methodSignature.getMethodName());

        // One-Way
        if (methodSignature.isOneway()) {
            LOG.debug("The method " + methodSignature.getMethodName() + " is marked 'oneway'.");
            AnnotationVisitor avOneway = methodVisitor.visitAnnotation(JSR181_ONEWAY, true);
            avOneway.visitEnd();
        }

        addAnnotationReturnName(methodVisitor, methodSignature.getReturnName());

        addParameterAnnotation(methodVisitor, methodSignature);

    }

    /**
     * This method is used to add the annotation to the method's parameters.
     *
     * @param    methodVisitor    The method visitor.
     *                            If null the annotation is not added.
     *
     * @param    sig              The method's signature.
     *                            If null the annotation is not added.
     *
     */
    protected void addParameterAnnotation(MethodVisitor methodVisitor,
            MethodSignature sig) {

        // check
        if (methodVisitor == null) {
            LOG.debug("The method visitor is null. add no annotation");
            return;
        }

        // check
        if (sig == null) {
            LOG.debug("The method's signature is null. add no annotation");
            return;
        }

        // check
        if (sig.getParameters() == null || sig.getParameters().size() == 0) {

            LOG.debug("The list of parameter name is null or empty." + "add no annotation");
            return;
        }


        LOG.debug("Adding parameter for the method: " + sig);
        for (int pos = 0; pos < sig.getParameters().size(); pos++) {

            String param = sig.getParameters().get(pos).getName();
            String paramType = sig.getParameters().get(pos).getTypeName();

            LOG.debug("Adding parameter name: " + param + " of type: " + paramType);

            AnnotationVisitor av = methodVisitor.visitParameterAnnotation(pos, JSR181_WEB_PARAM, true);
            av.visit("name", param);


            //av.visit("header", true); // default = false

            // If it's a Holder, adds the INOUT webparam mode
            if (sig.getParameters().get(pos).isHolder()) {
                av.visitEnum("mode", JSR181_WEB_PARAM_MODE, JSR181_WEB_PARAM_MODE_INOUT);
            }

            av.visitEnd();

            LOG.debug("WebParam annotation for [parameterName=" + param + "; parameterPosition=" + pos + "]");
        }


    }

    /**
     * This method is used to add an annotation to customize the name of the
     * return element.
     *
     * @param    methodVisitor    A method visitor.
     *                            If null the annotation is not added.
     *
     * @param    returnName        The new name.
     *                            If null or empty the annotation is not added.
     */
    protected void addAnnotationReturnName(MethodVisitor methodVisitor,
            String returnName) {

        // check
        if (methodVisitor == null) {
            LOG.debug("The method visitor is null. add no annotation");
            return;
        }

        // check
        if (returnName == null || "".equals(returnName)) {
            LOG.debug("The return name is null or empty. add no annotation");
            return;
        }

        // annotation
        AnnotationVisitor av = methodVisitor.visitAnnotation(JSR181_WEB_RESULT, true);

        av.visit("name", returnName);
        av.visitEnd();

    }

    /**
     * This method is used to add an annotation to customize the operation name.
     *
     * @param    methodVisitor    A method visitor.
     *                            If null the annotation is not added.
     *
     * @param    operationName    The new operation name.
     *                            If null or empty the annotation is not added.
     *
     */
    protected void addAnnotationOperationName(MethodVisitor methodVisitor,
            String operationName) {

        // check
        if (methodVisitor == null) {
            LOG.debug("The method visitor is null. add no annotation");
            return;
        }

        // check
        if (operationName == null || "".equals(operationName)) {
            LOG.debug("The operation name is null or empty. add no annotation");
            return;
        }

        // annotation
        AnnotationVisitor av = methodVisitor.visitAnnotation(JSR181_WEB_METHOD, true);

        av.visit("operationName", operationName);
        //avSig.visit("action", ""); // "" = default
        av.visitEnd();
    }

    /**
     * Find a method.
     *
     * @param    methodName    The finding key.
     *
     * @return    The method signature of the first element with the name
     *            in input. If the name is null or if the elemente does not exist
     *            the method return null.
     *
     */
    protected MethodSignature findFirstMethodSignature(String methodName) {
        if (methodSignatureList == null) {
            LOG.debug("Method's signature not found:" + "The list of the MethodSignature is null.");
            return null;
        }
        // else
        if (methodName == null) {
            LOG.debug("Method's signature not found:" + "The method name is null.");
            return null;
        }
        // else
        for (MethodSignature sig : methodSignatureList) {
            if (methodName.equals(sig.getMethodName())) {
                LOG.debug("MethodSignature FOUND. MethodName=" + methodName + "; MethodSignature=" + sig);
                return sig;
            }
        }
        // else
        LOG.debug("Method's signature not found for the method " + methodName);
        return null;
    }

    public Set<String> getExceptionsThrown() {
        return exceptionsThrown;
    }

    /**
     * Changes the method description and signature to change the method interface.
     * For example, the <br/>
     *    public String test(org.omg.CORBA.StringHolder arg, String nogeneric) <br/>
     *
     * Must be changed in: <br/>
     *
     *    public String test(javax.xml.ws.Holder<String> arg, String nogeneric) <br/>
     *
     *  To do so, the description must change in: <br/>
     *
     *    (Ljavax/xml/ws/Holder;Ljava/lang/String;)Ljava/lang/String; <br/>
     *
     *  And the Signature must be not null and: <br/>
     *
     *    (Ljavax/xml/ws/Holder<Ljava/lang/String;>;Ljava/lang/String;)Ljava/lang/String; <br/>
     *
     *
     * @param methodVisitor
     * @param sig
     * @param methodDescription
     * @param methodSignature
     */
    protected SignatureDescription changeHolderParameters(MethodSignature sig, String methodDescription, String methodSignature) {

        StringBuffer newMethodDescription = new StringBuffer("(");
        StringBuffer newMethodSignature = new StringBuffer("(");
        InternalMethodDescriptionParser parser = new InternalMethodDescriptionParser(methodDescription);
        List<String> internalParams = parser.parse();

        if (internalParams.size() != 0) {

            List<String> newParams = new ArrayList<String>();


            // Change the Holder Corba type to java.xml.ws.Holder
            for (int i = 0; i < sig.getParameters().size(); i++) {

                Param param = (Param) sig.getParameters().get(i);
                if (param.isHolder()) {
                    // Gets the paramt holder value type and converts it in the internal bytecode form.
                    Class paramClass = param.getHolderValueType();
                    String paramTypeDescriptor = Type.getDescriptor(paramClass);
                    String holderValueTypeStr = TypeUtils.getTypeNameWithoutBrackets(paramClass);
                    UnionType union = allUnionTypes.get(holderValueTypeStr);
                    InterfaceType intf = allInterfaceTypes.get(holderValueTypeStr);

                    String arrayStr = TypeUtils.getArrayDimmentionAsPrefix(paramClass);
                    boolean isAny = TypeUtils.getTypeNameWithoutBrackets(paramClass).equals(AnyType.CORBA_ANY_TYPE);

                    if (isAny) {
                        paramTypeDescriptor = arrayStr + "Ljava/lang/Object;";
                    }

                    if (intf != null) {
                        paramTypeDescriptor = W3CEPR;
                    }

                    if (union != null) {
                        String wrapperType = null;
                        try {
                            wrapperType = UnionTypeUtils.createUnionClassWrapper(union, allUnionTypes, workingDirClasses);
                            paramTypeDescriptor = arrayStr + "L" + wrapperType + ";";
                        } catch (ClassGenerationException e) {
                            LOG.error("Error generating wrapper class for uniontype:" + holderValueTypeStr);
                        }
                    }

                    LOG.debug("The parameter:" + param.getName() + "/" + param.getTypeName() + " is an Holder of type:" + paramTypeDescriptor);

                    // Sets the param  type  as javax.xml.ws.Holder
                    newParams.add(JAVAX_XML_WS_HOLDER_TYPE);
                    String newHolderParamName = JAVAX_XML_WS_HOLDER_TYPE_NO_SEMICOLON + "<" + paramTypeDescriptor + ">;";

                    // Sets javax.xml.Holder<class> as parameter type name.
                    param.setTypeName(JAVAX_XML_WS_HOLDER_TYPE_CANONICAL_NAME + "<" + param.getHolderValueType().getCanonicalName() + ">");

                    // Adds the generic to the signature:
                    newMethodSignature.append(newHolderParamName);

                } else {
                    // It's a "normal" (i mean no holder) parameter.
                    newParams.add(internalParams.get(i));
                    newMethodSignature.append(internalParams.get(i));
                }
                newMethodDescription.append(newParams.get(i));
            }

            newMethodDescription.append(")").append(parser.getMethodDescriptionTail());
            newMethodSignature.append(")").append(parser.getMethodDescriptionTail());
            ;
            // Reconstruct the correct method description
            methodDescription = newMethodDescription.toString();
            methodSignature = newMethodSignature.toString();
        }

        return new SignatureDescription(methodDescription, methodSignature);
    }

    /**
     * Wrapper class for the description and signature of the analized methods
     * @author marco
     *
     */
    public class SignatureDescription {

        // method description
        private String description;

        // method signature
        private String signature;

        public SignatureDescription(String desc, String sig) {
            description = desc;
            signature = sig;
        }

        public String getDescription() {
            return description;
        }

        public String getSignature() {
            return signature;
        }
    }
}
