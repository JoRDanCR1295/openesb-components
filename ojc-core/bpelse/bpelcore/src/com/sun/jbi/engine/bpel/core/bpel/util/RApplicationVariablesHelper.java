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
 * @(#)RApplicationVariablesHelper.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.util;

import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.parser.impl.ApplicationVariablesHelper;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.xml.common.visitor.VisitorService;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import java.util.HashSet;
import java.util.Set;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Vitaly Bychkov
 */
public class RApplicationVariablesHelper {

    private RApplicationVariablesHelper() {
    }

    public static AppVarValidationResult validateAppVarsAvailability(
            RBPELProcess bpelProcess, Map<String, Object> appVars) {
        assert bpelProcess != null;
        assert appVars != null;

        Set<String> unknownVars = new HashSet<String>();
        Set<AppVarValueValidationResult> appVarValueValResult =
                new HashSet<AppVarValueValidationResult>();

        VisitorService visitorService = new ValidateAppVarVisitorService();
        BPELVisitor visitor = (BPELVisitor) visitorService.fetch(BPELVisitor.class, null);
        visitor.reset();
        visitor.prepare(new Object[]{appVars, unknownVars, appVarValueValResult});


        bpelProcess.accept(visitor);

        AppVarValidationResult result = unknownVars != null || !appVarValueValResult.isEmpty() ?
            new AppVarValidationResultImpl(bpelProcess, unknownVars, appVarValueValResult) : null;

        return result;
    }

    public static Set<String> getUndefinedAppVars(Set<BPELProcess> bps, Set<String> registredAppVars) {
        Set<String> unknownAppVars = new HashSet<String>();
        Iterator<BPELProcess> bpItr = bps.iterator();
        while (bpItr.hasNext()) {
            unknownAppVars.addAll(getUndefinedAppVars(bpItr.next(), registredAppVars));
        }
        return unknownAppVars;
    }

    public static Set<String> getUndefinedAppVars(BPELProcess bpelProcess, Set<String> registredAppVars) {
        Set<String> unknownAppVars = new HashSet<String>();

        VisitorService visitorService = new ValidateAppVarVisitorService();
        BPELVisitor visitor = (BPELVisitor) visitorService.fetch(BPELVisitor.class, null);
        visitor.reset();
        visitor.prepare(new Object[]{registredAppVars, unknownAppVars});

        bpelProcess.accept(visitor);

        return unknownAppVars;
    }

    public static Set<String> getUsedAppVariables(String text) throws Exception {
        Set<String> usedVars = new HashSet<String>();

        TokenData tokenData = findNextToken(text, 0);
        while (tokenData != null) {
            String tokenName = tokenData.getTokenName();
            String appVariableName = getApplicationVariableName(tokenName);
            usedVars.add(appVariableName);

            if (tokenData.getEndIndex() < text.length()) {
                tokenData = findNextToken(text, tokenData.getEndIndex());
            } else {
                break;
            }
        }
        return usedVars;
    }

    public static Set<String> getUsedAppVariables(Element eII) throws Exception {
        Set<String> usedVars = new HashSet<String>();

        if (eII == null) {
            return usedVars;
        }

        NodeList children = eII.getChildNodes();
        if (children != null && children.getLength() > 0) {
            for (int i = 0; i < children.getLength(); i++) {
                Node child = children.item(i);

                short nodeType = child.getNodeType();
                if (Node.TEXT_NODE == nodeType || Node.COMMENT_NODE == nodeType || Node.CDATA_SECTION_NODE == nodeType) {
                    String nodeValue = child.getNodeValue();
                    if (nodeValue == null) {
                        continue;
                    }
                    usedVars.addAll(getUsedAppVariables(child.getNodeValue()));
                } else if (Node.ELEMENT_NODE == nodeType) {
                    usedVars.addAll(getUsedAppVariables((Element) child));
                }
            }
        }

        return usedVars;
    }

    /**
     * Return null or empty Set in case all variables were resolved
     * @param usedVars
     * @param appVars
     * @return
     */
    public static Set<String> getUnregistredVars(Set<String> usedVars, Set<String> appVars) {
        if (appVars == null || appVars.isEmpty() || usedVars == null || usedVars.isEmpty()) {
            return usedVars;
        }

        Set<String> unregistredVars = new HashSet<String>();
        for (String usedVar : usedVars) {
            if (!appVars.contains(usedVar)) {
                unregistredVars.add(usedVar);
            }
        }

        return unregistredVars;
    }

    public static String updateExprWithAppVars(Context variableScope, String expr) {
        try {
            Map<String, Object> appVars = variableScope.getProcessInstance().
                    getBPELProcessManager().getApplicationVariables();
            expr = replaceAppVar2Value(expr, appVars);
        } catch (Exception ex) {
            Logger.getLogger(Utility.class.getName()).log(Level.SEVERE, null, ex);
        }
        return expr;
    }

    public static Element replaceAppVar2Value(Element eII, Map mApplicationVariables) throws Exception {

        if (eII == null) {
            return eII;
        }

        NodeList nodeList = eII.getChildNodes();
        if (nodeList != null && nodeList.getLength() > 0) {
            for (int i = 0; i < nodeList.getLength(); i++) {
                Node childNode = nodeList.item(i);
                short nodeType = childNode.getNodeType();

                if (Node.TEXT_NODE == nodeType || Node.COMMENT_NODE == nodeType || Node.CDATA_SECTION_NODE == nodeType) {
                    String nodeValue = childNode.getNodeValue();
                    if (nodeValue == null) {
                        continue;
                    }
                    nodeValue = replaceAppVar2Value(nodeValue, mApplicationVariables);
                    childNode.setNodeValue(nodeValue);

                } else if (Node.ELEMENT_NODE == childNode.getNodeType()) {
                    assert childNode instanceof Element;
                    replaceAppVar2Value((Element) childNode, mApplicationVariables);
                }
            }
        }

        return eII;
    }

    public static String replaceAppVar2Value(String appVarString, Map mApplicationVariables) throws Exception {
        String newString = appVarString;
        Map unresolvedTokens = new HashMap();

        TokenData tokenData = findNextToken(newString, 0);
        while (tokenData != null) {
            String tokenName = tokenData.getTokenName();
            String tokenValue = "";


            String appVariableName = getApplicationVariableName(tokenName);
            String[] metadata = (String[]) mApplicationVariables.get(appVariableName);
            if (metadata == null || metadata[0] == null) {
                unresolvedTokens.put(tokenName, "");
                // update the application variable map as well
                mApplicationVariables.put(appVariableName, new String[]{null, "STRING"});
            } else {
                tokenValue = metadata[0];
            }

            // replace the token with actual value
            if (tokenValue != null) {
                newString = newString.replace(tokenName, tokenValue);
            }

            // update the start index to search for the next token
            tokenData.setEndIndex(tokenData.getStartIndex() + tokenValue.length());

            if (tokenData.getEndIndex() < newString.length()) {
                tokenData = findNextToken(newString, tokenData.getEndIndex());
            } else {
                break;
            }
        }

        if (unresolvedTokens.size() > 0) {
            StringBuffer unresolvedTokenNameList = new StringBuffer();
            for (Iterator it = unresolvedTokens.keySet().iterator(); it.hasNext();) {
                unresolvedTokenNameList.append((String) it.next() + " ");
            }
            throw new Exception(I18n.loc("BPCOR-7139: Found following tokens but no values are defined for them: {0}. Please make sure that these token values are configured properly and restart the application.", unresolvedTokenNameList.toString()));
        }
        return newString;
    }

    private static TokenData findNextToken(String aString, int aStartIndex) {
        TokenData retVal = null;
        int start = aString.indexOf(ApplicationVariablesHelper.TOKEN_START_SYMBOL, aStartIndex);
        int end = aString.indexOf(ApplicationVariablesHelper.TOKEN_END_SYMBOL, aStartIndex);
        if ((start >= 0 && end > 0) && (start < end)) {
            retVal = new TokenData(start, end + 1, aString.substring(start, end + 1));
        }

        return retVal;
    }

    private static String getApplicationVariableName(String aToken) throws Exception {
        String tokenName = null;

        if (aToken == null || "".equals(aToken)) {
            throw new Exception(I18n.loc("BPCOR-7140: Token name {0} is invalid", aToken));
        }

        tokenName = aToken.substring(ApplicationVariablesHelper.TOKEN_START_INDEX, aToken.length() - 1);
        if ("".equals(tokenName)) {
            throw new Exception(I18n.loc("BPCOR-7140: Token name {0} is invalid", aToken));
        }

        return tokenName;

    }

    private static class TokenData {

        private final String tokenName;
        private int startIndex;
        private int endIndex;

        public TokenData(int startIndex, int aIndex, String aName) {
            this.startIndex = startIndex;
            this.endIndex = aIndex;
            this.tokenName = aName;
        }

        public int getStartIndex() {
            return this.startIndex;
        }

        public int getEndIndex() {
            return this.endIndex;
        }

        public String getTokenName() {
            return this.tokenName;
        }

        public void setStartIndex(int index) {
            this.startIndex = index;
        }

        public void setEndIndex(int index) {
            this.endIndex = index;
        }
    }
}
