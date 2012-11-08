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
 * @(#)AspectSEEndpointHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.handler;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.lang.reflect.Constructor;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint;
import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint.EntryType;
import com.sun.jbi.engine.aspect.endpoint.factory.AspectMap;
import com.sun.jbi.engine.aspect.endpoint.support.ConfigObject;
import com.sun.jbi.engine.aspect.utils.AspectSEUtil;

/**
 * Base handler responsible for parsing the aspect map entry node and the
 * corresponding advice node
 *
 * @author Sujit Biswas
 *
 */
public abstract class AspectSEEndpointHandler {

    protected AspectSEEndpoint endpoint;

    protected ConfigObject configObj = new ConfigObject();

    protected String configFile;

    protected String rootPath;

    private static Logger logger = Logger
            .getLogger(AspectSEEndpointHandler.class.getName());

    public AspectSEEndpointHandler(AspectSEEndpoint ept) {
        this.endpoint = ept;
    }

    public void parse() {
        parseRoot();
        parseAdvice();
    }

    /**
     * parse the advice node, the advice node structure depends on the type of
     * advice or aspect under consideration, so its the responsibility of the
     * sub class to implement this method
     */
    protected abstract void parseAdvice();

    protected void parseRoot() {
        boolean invoke = false;
        rootPath = endpoint.getSURootPath();
        configFile = ((Element) endpoint.getAdvice())
                .getAttribute(AspectConstants.ADVICE_ATTR_CONFIG_FILE);
        Element root = endpoint.getRoot();
        String echangeType = root.getAttribute(AspectMap.EXCHANGE_TYPE);
        if (echangeType.equals(AspectMap.FILTER_REQUEST_REPLY_ELEMENT)) {
            endpoint.setEntryType(EntryType.FILTER_REQUEST_REPLY);
            invoke = true;
        } else if (echangeType.equals(AspectMap.FILTER_ONE_WAY_ELEMENT)) {
            endpoint.setEntryType(EntryType.FILTER_ONE_WAY);
            invoke = true;
        } else if (echangeType.equals(AspectMap.REQUEST_REPLY_ELEMENT)) {
            endpoint.setEntryType(EntryType.REQUEST_REPLY);
        } else {
            // TODO throw some exception
        }

        // Setting Input Invoke Points
        Element input = (Element) root.getElementsByTagName(
                AspectMap.INPUT_ELEMENT).item(0);

        initInputEndpoint(endpoint, input);
        endpoint.setId(root.getAttribute(AspectMap.ID_ATTR));

        // Creating Output Invoke Points
        if (endpoint.getNext() == null && invoke) {

            Element output = (Element) root.getElementsByTagName(
                    AspectMap.OUTPUT_ELEMENT).item(0);

            endpoint.setInvoke(createInvoke(output));
        }


        //add all the consuming endpoint if there are more than one
        NodeList nl = root.getElementsByTagName(AspectMap.OUTPUT_ELEMENT);

        if(nl.getLength() > 1){
            for( int i=1; i< nl.getLength(); i++ ){
                Element output = (Element)nl.item(i);
                endpoint.addInvoke(createInvoke(output));
            }
        }

    }

    /**
     *
     * @param ept
     * @param element
     */
    protected void initInputEndpoint(AspectSEEndpoint ept, Element element) {
        ept.setOperation(AspectSEUtil.convert(element.getAttribute(AspectMap.OPERATION_ATTR)));
        ept.setMessageType(AspectSEUtil.convert(element.getAttribute(AspectMap.MESSAGE_TYPE_ATTR)));

        // String transformJBI = element.getAttribute(AspectMap.CACHE_JBI_ATTR);
        // ept.setTemplates(AspectSEEndpointFactory.getTemplates(
        // ept.getRootPath(), element.getAttribute(AspectMap.FILE_ATTR)));

    }

    protected void initOutputEndpoint(AspectSEEndpoint ept, Element element) {
        ept.setOperation(AspectSEUtil.convert(element.getAttribute(AspectMap.OPERATION_ATTR)));
        Element output_rqst = (Element)element.getElementsByTagName(AspectMap.OUTPUT_REQUEST).item(0);
        ept.setMessageType(AspectSEUtil.convert(output_rqst.getAttribute(AspectMap.MESSAGE_TYPE_ATTR)));

        // String transformJBI = element.getAttribute(AspectMap.CACHE_JBI_ATTR);
        // ept.setTemplates(AspectSEEndpointFactory.getTemplates(
        // ept.getRootPath(), element.getAttribute(AspectMap.FILE_ATTR)));

    }


    protected AspectSEEndpoint createInvoke(Element element) {
        EndpointInfo info = null;
        if (element.getAttribute(AspectMap.PARTNERLINK_ATTR) == null
                || element.getAttribute(AspectMap.PARTNERLINK_ATTR).equals("")) {

            info = new EndpointInfo(false, String.valueOf(element
                    .getAttribute(AspectMap.PORT_NAME_ATTR)), AspectSEUtil
                    .convert(element.getAttribute(AspectMap.PORTTYPE_ATTR)),
                    AspectSEUtil.convert(element
                    .getAttribute(AspectMap.SERVICE_NAME_ATTR)), null); // no

            // link-type
        } else {
            info = new EndpointInfo(false, String.valueOf(element
                    .getAttribute(AspectMap.ROLE_NAME_ATTR)), AspectSEUtil
                    .convert(element.getAttribute(AspectMap.PORTTYPE_ATTR)),
                    AspectSEUtil.convert(element
                    .getAttribute(AspectMap.PARTNERLINK_ATTR)), null); // no
        }

        AspectSEEndpoint ept = null;
        try {
            Constructor[] cns = endpoint.getClass().getConstructors();

            Constructor c = null;

            for (int i = 0; i < cns.length; i++) {
                c = cns[i];
                Class[] ptypes = c.getParameterTypes();

                if (ptypes.length == 1 && (ptypes[0].isInstance(info))) {
                    break;
                }
            }
            ept = (AspectSEEndpoint) c.newInstance(new Object[] { info });
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        // AspectSEEndpoint ept = new AspectSEEndpoint(info);
        ept.setEntryType(endpoint.getEntryType());
        ept.setSURootPath(endpoint.getSURootPath());
        initOutputEndpoint(ept, element);
        ept.setId(element.getAttribute(AspectMap.ID_ATTR));
        return ept;
    }

    private ConfigObject getConfigObject() {
        return configObj;
    }

    /**
     * persist all the properties of the aspect endpoint in the corresponding
     * config file, This method will be called everytime the corresponding
     * endpoint properties are changed using config mbean
     *
     */
    public void save() {
        String xmlString = toXMLString();
        try {
            File confFile = new File(rootPath, configFile);
            if (!confFile.exists()) {
                confFile.createNewFile();
            }
            synchronized (this) {
                FileWriter writer = new FileWriter(confFile);
                writer.write(xmlString);
                writer.close();
            }
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    public void persistAspectMap() throws TransformerConfigurationException,
            TransformerException {
        Source source = new DOMSource(endpoint.getRoot().getParentNode());
        StringWriter stringWriter = new StringWriter();
        Result result = new StreamResult(stringWriter);
        endpoint.getTransformer().setOutputProperty(OutputKeys.INDENT, "yes");
        endpoint.getTransformer().transform(source, result);

        // Persist Aspect Map Here
        // System.out.println("-------> \n" +
        // stringWriter.getBuffer().toString());
        try {
            File suDescriptorFile = new File(rootPath,
                    AspectMap.DESCRIPTOR_FILE);
            if (!suDescriptorFile.exists()) {
                suDescriptorFile.createNewFile();
            }
            synchronized (this) {
                FileWriter writer = new FileWriter(suDescriptorFile);
                logger.log(Level.FINE, "Persisting changed to ["
                        + AspectMap.DESCRIPTOR_FILE + "] under : "
                        + suDescriptorFile.getAbsolutePath());
                writer.write(stringWriter.getBuffer().toString());
                writer.close();
            }
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    public String toXMLString() {
        StringBuffer xmlString = new StringBuffer();
        Element advice = endpoint.getAdvice();
        String name = advice.getAttribute(AspectConstants.ADVICE_ATTR_TYPE);
        xmlString.append("<" + name + ">\n");
        if (getConfigObject() != null) {
            xmlString.append(getConfigObject().toXMLString());
        } else {
            xmlString.append("<" + AspectConstants.CONFIG_TAG + "/>\n");
        }
        if (endpoint.getRuleset() != null) {
            xmlString.append(AspectSEUtil.xmlString(endpoint.getRuleset()));
        } else {
            xmlString.append("<" + AspectConstants.RULESET_TAG + "/>\n");
        }
        xmlString.append("</" + name + ">\n");
        return xmlString.toString();
    }
}
