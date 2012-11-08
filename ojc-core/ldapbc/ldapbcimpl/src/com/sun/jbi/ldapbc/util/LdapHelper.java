/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ldapbc.util;

import java.io.File;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;
import java.util.HashMap;
import javax.jbi.messaging.MessagingException;
import javax.naming.directory.SearchControls;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

import org.exolab.castor.xml.schema.Schema;
import org.exolab.castor.xml.schema.AttributeDecl;
import org.exolab.castor.xml.schema.ComplexType;
import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.Group;
import org.exolab.castor.xml.schema.reader.SchemaReader;
import org.xml.sax.EntityResolver;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.apache.xml.resolver.CatalogManager;

import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.ldapbc.LDAPBindingDeployer;
import com.sun.jbi.ldapbc.OperationMetaData;
import com.sun.jbi.ldapbc.extensions.LDAPAddress;
import com.sun.jbi.ldapbc.extensions.LDAPBinding;
import com.sun.jbi.ldapbc.extensions.LDAPExtensionRegistry;
import com.sun.jbi.ldapbc.extensions.LDAPOperation;
import com.sun.jbi.ldapbc.extensions.LDAPOperationInput;
import com.sun.jbi.ldapbc.extensions.LDAPOperationOutput;
import com.sun.jbi.ldapbc.extensions.LDAPExtPreprocessDeserializer;
import com.sun.jbi.ldapbc.update.UpdateBean;

/**
 *
 * @author Gary Zheng
 */
public class LdapHelper {

    private static final Logger mLogger = Messages.getLogger(LDAPBindingDeployer.class);

    public static SearchControls getSearchControl(Schema schema) throws MessagingException {
        ComplexType request = schema.getComplexType("RequestPropertyType");
        SearchControls ret = null;
        ElementDecl scopeElem = request.getElementDecl("scope");
        ElementDecl limitElem = request.getElementDecl("size");
        ElementDecl timeoutElem = request.getElementDecl("timeout");
        ElementDecl derefElem = request.getElementDecl("deref");
        //ElementDecl recordsPerPageElem = request.getElementDecl("recordsperpage");

        ret = new SearchControls();

        ret.setCountLimit(Long.valueOf(limitElem.getDefaultValue()));
        ret.setDerefLinkFlag(Boolean.valueOf(derefElem.getDefaultValue()));
        ret.setSearchScope(Integer.valueOf(scopeElem.getDefaultValue()));
        ret.setTimeLimit(Integer.valueOf(timeoutElem.getDefaultValue()));

        return ret;
    }

    public static String getReferral(Schema schema) throws MessagingException {
        ComplexType request = schema.getComplexType("RequestPropertyType");
        String ret = null;
        ElementDecl referralElem = request.getElementDecl("referral");

        ret = referralElem.getDefaultValue();

        return ret;
    }

    public static int getRecordsPerPage(Schema schema) throws MessagingException {
        ComplexType request = schema.getComplexType("RequestPropertyType");

        ElementDecl recordsPerPageElem = request.getElementDecl("recordsperpage");

        return Integer.valueOf(recordsPerPageElem.getDefaultValue()).intValue();
    }

    public static String getDN(Schema schema) throws MessagingException {
        ComplexType request = schema.getComplexType("RequestPropertyType");
        String ret = null;
        ElementDecl dnElem = request.getElementDecl("dn");

        ret = dnElem.getDefaultValue();

        return ret;
    }

    public static void setQueryAttributes(Schema schema, OperationMetaData meta) throws MessagingException {
        ComplexType searchFilterType = schema.getComplexType("SearchFilterType");
        int gCount = searchFilterType.getParticleCount();
        if (gCount < 0) {
            throw new MessagingException("LDAP schema searchFilterType have no sequence");
        }
        Group group = (Group) searchFilterType.getParticle(0);
        int eleCount = group.getParticleCount();
        for (int l = 0; l < eleCount; l++) {
            ElementDecl element = (ElementDecl) group.getParticle(l);
            String eleName = element.getName();
            int positionIndex = -1;
            int bracketDepth = -1;
            int bracketBeginDepth = -1;
            int bracketEndDepth = -1;
            String logicOp = "";
            String compareOp = "";
            Enumeration attrs = ((ComplexType) element.getType()).getAttributeDecls();
            while (attrs.hasMoreElements()) {
                AttributeDecl attr = (AttributeDecl) attrs.nextElement();
                String attrName = attr.getName();
                if (attrName.equals("positionIndex")) {
                    positionIndex = Integer.parseInt(attr.getFixedValue());
                    continue;
                }
                if (attrName.equals("bracketDepth")) {
                    bracketDepth = Integer.parseInt(attr.getFixedValue());
                    continue;
                }
                if (attrName.equals("bracketBeginDepth")) {
                    bracketBeginDepth = Integer.parseInt(attr.getFixedValue());
                    continue;
                }
                if (attrName.equals("bracketEndDepth")) {
                    bracketEndDepth = Integer.parseInt(attr.getFixedValue());
                    continue;
                }
                if (attrName.equals("logicOp")) {
                    logicOp = attr.getFixedValue();
                    continue;
                }
                if (attrName.equals("compareOp")) {
                    compareOp = attr.getFixedValue();
                }
            }
            element = null;
            meta.addAttribute(positionIndex, bracketDepth, bracketBeginDepth, bracketEndDepth, logicOp, compareOp, eleName);
        }
    }

    public static List<File> listXSDFiles(final File dir) {
        final List<File> cumulativeResults = new ArrayList<File>();
        final File[] filesInCurrentDir = dir.listFiles();

        for (File element : filesInCurrentDir) {

            if (element.isFile()) {

                if (element.getName().toLowerCase().endsWith(".xsd")) {
                    cumulativeResults.add(element);
                }
            } else if (element.isDirectory()) {
                final List<File> wsdlsInSubDirectories = listXSDFiles(element);
                cumulativeResults.addAll(wsdlsInSubDirectories);
            }
        }
        return cumulativeResults;
    }

    /**
     * 
     * @param f
     * @return
     * @throws java.lang.Exception
     */
    public static Schema readXsd(File f) throws Exception {
        SchemaReader reader = new SchemaReader(f);
        Schema schema = reader.read();
        return schema;
    }

    /**
     *
     * @param bindingOutput
     * @return
     */
    public static LDAPOperationOutput getLDAPOperationOutput(
            final BindingOutput bindingOutput) {
        LDAPOperationOutput operationOutput = null;

        final List extElems = bindingOutput.getExtensibilityElements();

        // Look for ldap:output entries
        final Iterator extIter = (extElems == null) ? null : extElems.iterator();

        while ((extIter != null) && extIter.hasNext()) {
            final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();

            if (LDAPOperationOutput.class.isInstance(ee)) {
                operationOutput = (LDAPOperationOutput) ee;

                break;
            }
        }

        return operationOutput;
    }

    public static Map<String, String[]> parseForEnvironmentVariables(final File suDir, final Map<String, String[]> envVariableMap) throws Exception {
        Map<String, String[]> envVariables =
                new HashMap<String, String[]>(envVariableMap);

        File catalog = new File(suDir.getAbsolutePath() + File.separator + "meta-inf" + File.separator + "catalog.xml");

        EntityResolver resolver = null;

        if (catalog.exists()) {
            CatalogManager catalogManager = new CatalogManager();
            catalogManager.setCatalogFiles(catalog.getAbsolutePath());
            catalogManager.setRelativeCatalogs(true);
            catalogManager.setUseStaticCatalog(false);
            resolver = new CatalogResolver(catalogManager);
        }

        for (File file : listWSDLFiles(suDir)) {
            envVariables.putAll(
                    readWSDLForEnvVariables(file,
                    resolver,
                    envVariables));
        }

        return envVariables;
    }

    public static Map readWSDLForEnvVariables(final File f, final EntityResolver resolver,
            final Map<String, String[]> envVariableMap) throws WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = resolver == null ? ((WSDLFactoryImpl) wsdlFactory).newWSDLReader() : ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);

        LDAPExtPreprocessDeserializer preProcessDeserializer = new LDAPExtPreprocessDeserializer(
                envVariableMap);
        reader.setExtensionRegistry(new LDAPExtensionRegistry(
                preProcessDeserializer));
        reader.readWSDL(f.getAbsolutePath());
        return preProcessDeserializer.getEnvVariableMap();
    }

    public static List<File> listWSDLFiles(final File currentDir) {
        final List<File> cumulativeResults = new ArrayList<File>();
        final File[] filesInCurrentDir = currentDir.listFiles();

        for (File element : filesInCurrentDir) {

            if (element.isFile()) {

                if (element.getName().toLowerCase().endsWith(".wsdl")) {
                    cumulativeResults.add(element);
                }
            } else if (element.isDirectory()) {
                final List<File> wsdlsInSubDirectories = listWSDLFiles(element);
                cumulativeResults.addAll(wsdlsInSubDirectories);
            }
        }
        return cumulativeResults;
    }

    /**
     *
     * @param f
     * @param resolver
     * @return
     * @throws javax.wsdl.WSDLException
     */
    /*   public static Definition readWsdl(final File f, final EntityResolver resolver)
    throws javax.wsdl.WSDLException {
    final WSDLFactory wsdlFactory = WSDLFactory.newInstance();
    final WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
    reader.setExtensionRegistry(new LDAPExtensionRegistry());
    
    final Definition def = reader.readWSDL(f.getAbsolutePath());
    
    
    return def;
    } */
    public static Definition readWSDL(final File f, final EntityResolver resolver,
            final Map<String, String[]> envVariableMap) throws WSDLException {
        final WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        final WSDLReader reader = resolver == null ? ((WSDLFactoryImpl) wsdlFactory).newWSDLReader() : ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        reader.setExtensionRegistry(new LDAPExtensionRegistry(envVariableMap));
        final Definition def = reader.readWSDL(f.getAbsolutePath());

        return def;
    }

    /**
     *
     * @param def
     * @param serviceName
     * @param endpointName
     * @return
     */
    public static Binding getBinding(final Definition def, final String serviceName,
            final String endpointName) {
        final Service svc = def.getService(QName.valueOf(serviceName));

        if (svc == null) {
            return null;
        }

        final Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());

        if (port == null) {
            return null;
        } else {
            return port.getBinding();
        }
    }

    /**
     *
     * @param def
     * @param portTypeName
     * @param operationName
     * @return
     */
    public static Operation getOperation(final Definition def, final String portTypeName,
            final String operationName) {
        Operation operation = null;
        final PortType pType = def.getPortType(QName.valueOf(portTypeName));

        if (pType != null) {
            operation = pType.getOperation(operationName, null, null);
        }

        return operation;
    }

    /**
     *
     * @param def
     * @param serviceName
     * @param endpointName
     * @return
     */
    public static LDAPAddress getLDAPAddress(final Definition def, final String serviceName,
            final String endpointName) {
        LDAPAddress address = null;
        final Service svc = def.getService(QName.valueOf(serviceName));

        if (svc == null) {
            return null;
        }

        final Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());

        if (port != null) {
            final List extElems = port.getExtensibilityElements();

            // Look for ldap:address
            final Iterator extIter = (extElems == null) ? null : extElems.iterator();

            while ((extIter != null) && extIter.hasNext() &&
                    (address == null)) {
                final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();

                if (LDAPAddress.class.isInstance(ee)) {
                    address = (LDAPAddress) ee;
                }
            }
        }

        return address;
    }

    /**
     *
     * @param def
     * @param serviceName
     * @param endpointName
     * @return
     */
    public static LDAPBinding getLDAPBinding(final Definition def, final String serviceName,
            final String endpointName) {
        LDAPBinding ldapBinding = null;
        final Binding binding = getBinding(def, serviceName, endpointName);

        if (binding != null) {
            final List extElems = binding.getExtensibilityElements();

            // Look for ldap:binding
            final Iterator extIter = (extElems == null) ? null : extElems.iterator();

            while ((extIter != null) && extIter.hasNext() &&
                    (ldapBinding == null)) {
                final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();

                if (LDAPBinding.class.isInstance(ee)) {
                    ldapBinding = (LDAPBinding) ee;
                }
            }
        }

        return ldapBinding;
    }

    /**
     *
     * @param def
     * @param serviceName
     * @param endpointName
     * @return
     */
    public static LDAPOperation[] getLDAPOperations(final Definition def,
            final String serviceName, final String endpointName) {
        final List<ExtensibilityElement> ldapOperations = new ArrayList<ExtensibilityElement>();
        final Binding binding = LdapHelper.getBinding(def, serviceName, endpointName);

        if (binding != null) {
            final List bindingOperations = binding.getBindingOperations();
            final Iterator operIter = (bindingOperations == null) ? null
                    : bindingOperations.iterator();

            while ((operIter != null) && operIter.hasNext()) {
                final BindingOperation oper = (BindingOperation) operIter.next();
                final List extElems = oper.getExtensibilityElements();

                // Look for file:operation entries
                final Iterator extIter = (extElems == null) ? null
                        : extElems.iterator();

                while ((extIter != null) && extIter.hasNext()) {
                    final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();

                    if (LDAPOperation.class.isInstance(ee)) {
                        ldapOperations.add(ee);
                    }
                }
            }
        }

        return ldapOperations.toArray(new LDAPOperation[0]);
    }

    /**
     *
     * @param bindingInput
     * @return
     */
    public static LDAPOperationInput getLDAPOperationInput(
            final BindingInput bindingInput) {
        LDAPOperationInput operationInput = null;
        final List extElems = bindingInput.getExtensibilityElements();

        // Look for ldap:input entries
        final Iterator extIter = (extElems == null) ? null : extElems.iterator();

        while ((extIter != null) && extIter.hasNext()) {
            final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();

            if (LDAPOperationInput.class.isInstance(ee)) {
                operationInput = (LDAPOperationInput) ee;

                break;
            }
        }

        return operationInput;
    }

    /**
     *
     * @param ldapOperation
     * @param def
     * @param serviceName
     * @param endpointName
     * @return
     */
    public static BindingOperation getParentBindingOperation(
            final LDAPOperation ldapOperation, final Definition def, final String serviceName,
            final String endpointName) {
        BindingOperation bindingOperation = null;

        final Binding binding = LdapHelper.getBinding(def, serviceName, endpointName);

        if (binding != null) {
            final List l = binding.getBindingOperations();
            final Iterator it = l.iterator();

            while (it.hasNext()) {
                final BindingOperation bOperation = (BindingOperation) it.next();
                final List exElements = bOperation.getExtensibilityElements();

                if ((exElements != null) &&
                        exElements.contains(ldapOperation)) {
                    bindingOperation = bOperation;

                    break;
                }
            }
        }

        return bindingOperation;
    }

    public static Schema getSchema(String nsUri, File dir) {
        Schema ret = null;
        List<File> xsdFileList = LdapHelper.listXSDFiles(dir);
        for (int k = 0; k < xsdFileList.size(); k++) {
            File f = xsdFileList.get(k);
            String uri = "";
            try {
                Schema schema = LdapHelper.readXsd(f);
                uri = schema.getTargetNamespace();
                if (nsUri.equals(uri)) {
                    ret = schema;
                    break;
                }
            } catch (Exception ex) {
                LdapHelper.mLogger.log(Level.SEVERE, null, ex);
            }
        }

        return ret;
    }

    public static List<String> getResponseAttributes(Schema schema) throws MessagingException {
        List<String> ret = new ArrayList<String>();
        ComplexType responseType = schema.getComplexType("ResponseEntryType");
        Group resGroup = (Group) responseType.getParticle(0);
        int resGroupCount = resGroup.getParticleCount();
        if (resGroupCount < 1) {
            throw new MessagingException("LDAP schema ResponseType have no response element");
        }
        for (int n = 0; n < resGroupCount; n++) {
            ElementDecl resEle = (ElementDecl) resGroup.getParticle(n);
            ret.add(resEle.getName());
        }

        return ret;
    }

    public static void setUpdateAttributes(Schema schema, OperationMetaData opMetaData) throws MessagingException {
        ComplexType updateType = schema.getComplexType("UpdateElementsType");
        Group updateGroup = (Group) updateType.getParticle(0);
        int updateGroupCount = updateGroup.getParticleCount();
        if (updateGroupCount < 1) {
            throw new MessagingException("LDAP schema UpdateElementsType have no update element");
        }
        for (int l = 0; l < updateGroupCount; l++) {
            ElementDecl element = (ElementDecl) updateGroup.getParticle(l);
            String eleName = element.getName();
            int index = eleName.indexOf(".");
            String objName = eleName.substring(0, index);
            String updateAttrName = eleName.substring(index + 1);
            String opType = "";
            Enumeration attrs = ((ComplexType) element.getType()).getAttributeDecls();
            while (attrs.hasMoreElements()) {
                AttributeDecl attr = (AttributeDecl) attrs.nextElement();
                String attrName = attr.getName();
                if ("opType".equals(attrName)) {
                    opType = attr.getFixedValue();
                }
                if ("".equals(opType)) {
                    throw new MessagingException("Can't get opType from schema");
                }
                UpdateBean updateBean = new UpdateBean(objName, updateAttrName, opType);
                opMetaData.addUpdateAttribute(eleName, updateBean);
            }
            element = null;
        }
    }

    public static List<String> getAddedReturnAttrs(Schema schema) throws MessagingException {
        List<String> ret = new ArrayList<String>();
        ComplexType responseType = schema.getComplexType("AddReturnAttributesType");
        Group resGroup = (Group) responseType.getParticle(0);
        int resGroupCount = resGroup.getParticleCount();
        if (resGroupCount < 1) {
            throw new MessagingException("LDAP schema ResponseType have no response element");
        }
        for (int n = 0; n < resGroupCount; n++) {
            ElementDecl resEle = (ElementDecl) resGroup.getParticle(n);
            ret.add(resEle.getName());
        }

        return ret;
    }

    public static String getSortedByAttribute(Schema schema) throws MessagingException {
        ComplexType request = schema.getComplexType("RequestPropertyType");

        ElementDecl recordsPerPageElem = request.getElementDecl("sortbyattribute");

        return recordsPerPageElem.getDefaultValue();
    }

    public static String getSortedByType(Schema schema) throws MessagingException {
        ComplexType request = schema.getComplexType("RequestPropertyType");

        ElementDecl recordsPerPageElem = request.getElementDecl("sorttype");

        return recordsPerPageElem.getDefaultValue();
    }
}
