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

package com.sun.jbi.component.toolkit.util;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchange.Role;

/**
 * Enum representing legal combinations of {@link ExchangePattern},
 * {@link Role}, and {@link ExchangeStatus}.
 * 
 * @author Kevan Simpson
 */
public enum PatternRoleStatus {
//    IN_ONLY_CONSUMER_ACTIVE - never happens
    IN_ONLY_CONSUMER_DONE(ExchangePattern.IN_ONLY, Role.CONSUMER, ExchangeStatus.DONE),
    IN_ONLY_CONSUMER_ERROR(ExchangePattern.IN_ONLY, Role.CONSUMER, ExchangeStatus.ERROR),
    IN_ONLY_PROVIDER_ACTIVE(ExchangePattern.IN_ONLY, Role.PROVIDER, ExchangeStatus.ACTIVE),
//    IN_ONLY_PROVIDER_DONE - never happens
//    IN_ONLY_PROVIDER_ERROR - never happens
    IN_OPT_OUT_CONSUMER_ACTIVE(ExchangePattern.IN_OPTIONAL_OUT, Role.CONSUMER, ExchangeStatus.ACTIVE),
    IN_OPT_OUT_CONSUMER_DONE(ExchangePattern.IN_OPTIONAL_OUT, Role.CONSUMER, ExchangeStatus.DONE),
    IN_OPT_OUT_CONSUMER_ERROR(ExchangePattern.IN_OPTIONAL_OUT, Role.CONSUMER, ExchangeStatus.ERROR),
    IN_OPT_OUT_PROVIDER_ACTIVE(ExchangePattern.IN_OPTIONAL_OUT, Role.PROVIDER, ExchangeStatus.ACTIVE),
    IN_OPT_OUT_PROVIDER_DONE(ExchangePattern.IN_OPTIONAL_OUT, Role.PROVIDER, ExchangeStatus.DONE),
    IN_OPT_OUT_PROVIDER_ERROR(ExchangePattern.IN_OPTIONAL_OUT, Role.PROVIDER, ExchangeStatus.ERROR),
    IN_OUT_CONSUMER_ACTIVE(ExchangePattern.IN_OUT, Role.CONSUMER, ExchangeStatus.ACTIVE),
//    IN_OUT_CONSUMER_DONE - never happens
    IN_OUT_CONSUMER_ERROR(ExchangePattern.IN_OUT, Role.CONSUMER, ExchangeStatus.ERROR),
    IN_OUT_PROVIDER_ACTIVE(ExchangePattern.IN_OUT, Role.PROVIDER, ExchangeStatus.ACTIVE),
    IN_OUT_PROVIDER_DONE(ExchangePattern.IN_OUT, Role.PROVIDER, ExchangeStatus.DONE),
    IN_OUT_PROVIDER_ERROR(ExchangePattern.IN_OUT, Role.PROVIDER, ExchangeStatus.ERROR),
    ROBUST_IN_ONLY_CONSUMER_ACTIVE(ExchangePattern.ROBUST_IN_ONLY, Role.CONSUMER, ExchangeStatus.ACTIVE),
    ROBUST_IN_ONLY_CONSUMER_DONE(ExchangePattern.ROBUST_IN_ONLY, Role.CONSUMER, ExchangeStatus.DONE),
    ROBUST_IN_ONLY_CONSUMER_ERROR(ExchangePattern.ROBUST_IN_ONLY, Role.CONSUMER, ExchangeStatus.ERROR),
    ROBUST_IN_ONLY_PROVIDER_ACTIVE(ExchangePattern.ROBUST_IN_ONLY, Role.PROVIDER, ExchangeStatus.ACTIVE),
    ROBUST_IN_ONLY_PROVIDER_DONE(ExchangePattern.ROBUST_IN_ONLY, Role.PROVIDER, ExchangeStatus.DONE),
    ROBUST_IN_ONLY_PROVIDER_ERROR(ExchangePattern.ROBUST_IN_ONLY, Role.PROVIDER, ExchangeStatus.ERROR);

    
    private final ExchangePattern mPattern;
    private final Role mRole;
    private final ExchangeStatus mStatus;
    
    PatternRoleStatus(ExchangePattern pattern, Role role, ExchangeStatus status) {
        mPattern = pattern;
        mRole = role;
        mStatus = status;
    }
    
    public ExchangePattern getPattern() { 
        return mPattern; 
    }
    
    public Role getRole() { 
        return mRole; 
    }
    
    public ExchangeStatus getStatus() {
        return mStatus;
    }
    
    /**
     * Resolves the specified {@link MessageExchange} to a {@link PatternRoleStatus} enum.
     * 
     * @param msg A message exchange.
     * @return A <code>PatternRoleKey</code> enum or <code>null</code>.
     */
    public static PatternRoleStatus valueOf(MessageExchange msg) {
        if (msg == null) return null;
        
        ExchangePattern patt =  ExchangePattern.valueOf(msg); 
        for (PatternRoleStatus prs : PatternRoleStatus.values()) {
            // an enum, '==' will suffice below
            if (prs.getPattern() == patt) {
                if (msg.getRole().equals(prs.getRole())) {
                    if (msg.getStatus().equals(prs.getStatus())) {
                        return prs;
                    }
                }
            }
        }
        
        return null;
    }
}
