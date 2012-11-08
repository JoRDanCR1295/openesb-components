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
 */

package com.sun.jbi.ldapbc.configuration;

import com.sun.jbi.ldapbc.extensions.LDAPAddress;

/**
 * A Visitor implementation interested in configuring LDAPAddress objects.
 * 
 * @author Noel.Ang@sun.com
 */
public class AppConfigAddressVisitor implements Visitor {
    private final LDAPAddress mAddress;

    public AppConfigAddressVisitor(LDAPAddress address) {
        assert address != null;
        mAddress = address;
    }

    public void visit(Visitable visitable) {
        synchronized (mAddress) {
            if (visitable instanceof AppConfigLocationField) {
                AppConfigLocationField field = (AppConfigLocationField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());
            } else if (visitable instanceof AppConfigPrincipalField) {
                AppConfigPrincipalField field = (AppConfigPrincipalField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());
            } else if (visitable instanceof AppConfigCredentialField) {
                AppConfigCredentialField field = (AppConfigCredentialField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());
            } else if (visitable instanceof AppConfigSSLTypeField) {
                AppConfigSSLTypeField field = (AppConfigSSLTypeField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());
            } else if (visitable instanceof AppConfigAuthenticationField) {
                AppConfigAuthenticationField field = (AppConfigAuthenticationField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());
            } else if (visitable instanceof AppConfigProtocolField) {
                AppConfigProtocolField field = (AppConfigProtocolField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());          
			} else if (visitable instanceof AppConfigTrustStoreField) {
                AppConfigTrustStoreField field = (AppConfigTrustStoreField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());
            }  else if (visitable instanceof AppConfigTrustStorePwdField) {
                AppConfigTrustStorePwdField field = (AppConfigTrustStorePwdField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());           
			} else if (visitable instanceof AppConfigTrustStoreTypeField) {
                AppConfigTrustStoreTypeField field = (AppConfigTrustStoreTypeField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());          
			} else if (visitable instanceof AppConfigKeystoreField) {
                AppConfigKeystoreField field = (AppConfigKeystoreField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());           
			} else if (visitable instanceof AppConfigKeystorePwdField) {
                AppConfigKeystorePwdField field = (AppConfigKeystorePwdField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());            
			} else if (visitable instanceof AppConfigKeystoreUNameField) {
                AppConfigKeystoreUNameField field = (AppConfigKeystoreUNameField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());           
			} else if (visitable instanceof AppConfigTrustStoreTypeField) {
                AppConfigTrustStoreTypeField field = (AppConfigTrustStoreTypeField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());            
			} else if (visitable instanceof AppConfigTLSSecurityField) {
                AppConfigTLSSecurityField field = (AppConfigTLSSecurityField) visitable;
                mAddress.setProperty(field.getFieldId(), field.getValue());
            }
        }
    }
}
