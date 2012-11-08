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
 * @(#)ThrottleInput.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.support;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;

/**
 * This class is like a bean that manages the inpus to the throttle with their contexts
 * @author bharanim
 */
public class ThrottleInput {
    
    private CRLInOut inOut = null;
    private CRLInOnly inOnly = null;
    private ExchangeContext ctx = null;
    
    /** Creates a new instance of ThrottleInput */
    public ThrottleInput() {
    }
    
    public ThrottleInput(CRLInOnly inOnly, ExchangeContext ctx)
    {
        this.inOnly = inOnly;
        this.ctx = ctx;
    }    
    
    public ThrottleInput(CRLInOut inOut, ExchangeContext ctx)
    {
        this.inOut = inOut;
        this.ctx = ctx;
    }
    
    public CRLInOnly getCRLInOnly()
    {
        return this.inOnly;
    }
    
    public void setCRLInOnly(CRLInOnly inOnly)
    {
        this.inOnly = inOnly;
    }    
    
    public CRLInOut getCRLInOut()
    {
        return this.inOut;
    }
    
    public void setCRLInOut(CRLInOut inOut)
    {
        this.inOut = inOut;
    }
    
    public ExchangeContext getExchangeContext()
    {
        return this.ctx;
    }
    
    public void setExchangeContext(ExchangeContext ctx)
    {
        this.ctx = ctx;
    }
    
}
