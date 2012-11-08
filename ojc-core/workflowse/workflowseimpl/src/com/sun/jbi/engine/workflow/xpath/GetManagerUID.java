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
 * @(#)$Id: GetManagerUID.java,v 1.3 2010/02/15 19:24:47 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.xpath;

import java.util.logging.Logger;

import org.apache.commons.jxpath.ExpressionContext;
import org.apache.commons.jxpath.Function;
import org.apache.commons.jxpath.JXPathContext;

import com.sun.jbi.engine.workflow.process.LDAPConfig;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.engine.workflow.util.LDAPSearchUtil;
import com.sun.jbi.workflow.model.RuntimeVariables;

public class GetManagerUID implements Function {
    private static final Logger LOGGER = Logger.getLogger(GetManagerUID.class.getName());

    public Object invoke(ExpressionContext ctx, Object[] params) {
        // TODO Auto-generated method stub
        JXPathContext context = ctx.getJXPathContext();
        LdapXpathContext ldapctx = (LdapXpathContext) context;
        LDAPConfig ldapConfig = ldapctx.getLdapConfig();
        if (ldapConfig == null || ldapConfig.getLDAPContext() == null) {
            throw new XpathException (I18n.loc(
                    "WLM-6088:  No LDAP connection is available"));            
        }        
        String userOrGroup = null;
        if (params == null || params.length == 0) {
            userOrGroup = (String) context.getVariables().getVariable(RuntimeVariables.TASK_INSTANCE_OWNER);
            if (userOrGroup == null) {
                throw new XpathException (I18n.loc(
                        "WLM-6085:  get-manager-uid has no arguments and task is not claimed by anyone"));
            }
        }else {
            Object param0 = params[0];
            if (param0 instanceof String) {
                userOrGroup = (String) params [0];
            } else if (param0 instanceof ExpressionContext) {
                userOrGroup = (String) ((ExpressionContext) param0).getContextNodePointer().getValue();
            }
        }
        String uid = null;
        String managerDN = null;
        //Try to get manager for the group, if not found, try to get the manager for the user
        try {
            managerDN = LDAPSearchUtil.getAttributeByUID(ldapConfig.getGroupFilter(), userOrGroup,
                    ldapConfig.getManagerAttName(), ldapConfig.getLDAPContext(), ldapConfig.getBaseDN(), ldapConfig.getScope());
            if (managerDN == null) {
                managerDN = LDAPSearchUtil.getAttributeByUID(ldapConfig.getUserFilter(), userOrGroup,
                        ldapConfig.getManagerAttName(), ldapConfig.getLDAPContext(), ldapConfig.getBaseDN(), ldapConfig.getScope());                
            }
            if (managerDN == null) {
                LOGGER.warning(I18n.loc(
                        "WLM-6086: Can not get manager uid for {0}, Ldap config : {1}", userOrGroup, ldapConfig.toString()
                        ));             
                return null;
            }
            
            //Get the email from DN
            uid =  LDAPSearchUtil.getAttributeByDN(managerDN, ldapConfig.getUIDAttName(),  ldapConfig.getLDAPContext());
            
            if (uid == null) {
                LOGGER.warning(I18n.loc(
                        "WLM-6086: Can not get manager uid for {0}, Ldap config : {1}", userOrGroup, ldapConfig.toString()
                        ));             
                return null;
            }
        } catch (Exception e) {
            // TODO Auto-generated catch block
            throw new XpathException (I18n.loc(
                    "WLM-6086: Can not get manager uid for {0}, Ldap config : {1}", userOrGroup, ldapConfig.toString()
                    ), e);
        }        
        return uid;  
    }



}
