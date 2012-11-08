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
 * Copyright 2007-2008 ZAZ Consulting, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.zaz.ssapi.protocol.common.util;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;

/**
 *
 * @author tianlize
 */
public class EmulatorScriptMapGenerator {

    public EmulatorScriptMapGenerator() {
    }

    public static void writeScriptMap(File dir, String opName) {
        try {
            Writer output = new BufferedWriter(new FileWriter(dir.getPath() +
                    File.separator + "src" + File.separator +
                    "scriptmap.xml"));

            output.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + "\n");
            output.write(
                    "<scriptconfig xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"C:\\Alaska\\jbicomps\\ScriptSE\\schema\\ScriptMap.xsd\">" +
                    "\n");
            output.write("    <ScrEngineDetails>" + "\n");
            output.write("        <enginename>JRuby Engine</enginename>" +
                    "\n");
            output.write("        <extension>String</extension>" + "\n");
            output.write("        <version>String</version>" + "\n");
            output.write("        <mimetype>String</mimetype>" + "\n");
            output.write("    </ScrEngineDetails>" + "\n");
            output.write("    <operations>" + "\n");
            output.write("        <operation name=\"" + "invoke_function_xxx" +
                    "\" wsdlop=\"" + opName + "Operation\">" + "\n");
            output.write(
                    "            <parameter name=\"request\" type=\"xsdType\" xsdname=\"" +
                    opName + ".xsd\"/>" + "\n");
            output.write(
                    "            <returntype name=\"response\" type=\"xsdType\" xsdname=\"org.netbeans.xml.schema." +
                    opName.toLowerCase() + ".Response\"/>" + "\n");
            output.write("        </operation>" + "\n");
            output.write("    </operations>" + "\n");
            output.write("    <requestReplyService>" + "\n");
            output.write("        <input file=\"" + opName +
                    ".rb\" messageType=\"{http://j2ee.netbeans.org/wsdl/" + opName +
                    "}" + opName + "OperationResponse\" operation=\"" + opName +
                    "Operation\" partnerLink=\"{http://j2ee.netbeans.org/wsdl/" +
                    opName + "PartnerLink}" + opName +
                    "\" portType=\"{http://j2ee.netbeans.org/wsdl/" + opName + "" +
                    "}" + opName + "PortType\" roleName=\"" + opName +
                    "PortTypeRole\" transformJBI=\"false\"/>" + "\n");
            output.write("    </requestReplyService>" + "\n");
            output.write("</scriptconfig>" + "\n");

            output.close();
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } catch (IOException io) {
            io.printStackTrace();
        }
    }
}
