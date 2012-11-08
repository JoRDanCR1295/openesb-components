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
 * @(#)PatternRoleKey.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.scriptse.jbiadapter;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchange.Role;

import com.sun.jbi.component.toolkit.util.ExchangePattern;

/**
 * Enum representing pattern-role based {@link RouterKey}s.
 * 
 * @author Kevan Simpson
 */
public enum PatternRole {
    IN_ONLY_CONSUMER(ExchangePattern.IN_ONLY, Role.CONSUMER),
    IN_ONLY_PROVIDER(ExchangePattern.IN_ONLY, Role.PROVIDER),
    IN_OPT_OUT_CONSUMER(ExchangePattern.IN_OPTIONAL_OUT, Role.CONSUMER),
    IN_OPT_OUT_PROVIDER(ExchangePattern.IN_OPTIONAL_OUT, Role.PROVIDER),
    IN_OUT_CONSUMER(ExchangePattern.IN_OUT, Role.CONSUMER),
    IN_OUT_PROVIDER(ExchangePattern.IN_OUT, Role.PROVIDER),
    ROBUST_IN_ONLY_CONSUMER(ExchangePattern.ROBUST_IN_ONLY, Role.CONSUMER),
    ROBUST_IN_ONLY_PROVIDER(ExchangePattern.ROBUST_IN_ONLY, Role.PROVIDER);
    
    private ExchangePattern mPattern = null;
    private Role mRole = null;
    
    PatternRole(ExchangePattern pattern, Role role) {
        mPattern = pattern;
        mRole = role;
    }
    
    public ExchangePattern getPattern() { 
        return mPattern; 
    }
    
    public Role getRole() { 
        return mRole; 
    }
    
    /**
     * Resolves the specified {@link MessageExchange} to a {@link PatternRole} enum.
     * 
     * @param msg A message exchange.
     * @return A <code>PatternRoleKey</code> enum or <code>null</code>.
     */
    public static PatternRole toKey(MessageExchange msg) {
        if (msg == null) return null;
        
        ExchangePattern patt =  ExchangePattern.valueOf(msg); 
        for (PatternRole key : PatternRole.values()) {
            // an enum, '==' will suffice below
            if (key.getPattern() == patt) {
                if (msg.getRole().equals(key.getRole())) {
                    return key;
                }
            }
        }
        
        return null;
    }
}
