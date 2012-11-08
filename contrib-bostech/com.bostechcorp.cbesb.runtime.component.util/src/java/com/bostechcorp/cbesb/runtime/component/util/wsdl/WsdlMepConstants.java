/*
 * ChainBuilder ESB
 *          Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
 * 
 * This program is free software; you can redistribute it and/or modify it 
 * under the terms of the GNU General Public License as published by the 
 * Free Software Foundation; either version 2 of the License, or (at your option) 
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along with 
 * this program; if not, write to the Free Software Foundation, Inc., 
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *
 * $Id: WsdlMepConstants.java,v 1.1.1.1 2007/04/09 17:49:31 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.component.util.wsdl;

import java.net.URI;

public class WsdlMepConstants  {


    public static final String DEFAULT_MEP = "defaultMep";
    
    public static final String DEFAULT_MEP_IN_ONLY = "in-only";
    public static final String DEFAULT_MEP_ROBUST_IN_ONLY = "robust-in-only";
    public static final String DEFAULT_MEP_IN_OUT = "in-out";
    
    public static final String WSDL2_NS = "http://www.w3.org/2004/08/wsdl/";
    
    
    /**
     * In Only MEP.
     */
    public static final URI IN_ONLY = URI.create(WSDL2_NS + DEFAULT_MEP_IN_ONLY);
    /**
     * In Out MEP.
     */
    public static final URI IN_OUT = URI.create(WSDL2_NS + DEFAULT_MEP_IN_OUT);
    /**
     * In Optional Out MEP.
     */
    public static final URI IN_OPTIONAL_OUT = URI.create(WSDL2_NS + "in-optional-out");
    /**
     * Robust In Only MEP.
     */
    public static final URI ROBUST_IN_ONLY = URI.create(WSDL2_NS + DEFAULT_MEP_ROBUST_IN_ONLY);
    /**
     * Out Only MEP.
     */
    public static final URI OUT_ONLY = URI.create(WSDL2_NS + "out-only");
    /**
     * Out In MEP.
     */
    public static final URI OUT_IN = URI.create(WSDL2_NS + "out-in");
    /**
     * Out Optional In MEP.
     */
    public static final URI OUT_OPTIONAL_IN = URI.create(WSDL2_NS + "out-opt-in");
    /**
     * Robust Out Only MEP.
     */
    public static final URI ROBUST_OUT_ONLY = URI.create(WSDL2_NS + "robust-out-only");
    
   

}
