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
 * @(#)EmailAddress.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2003, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.smtpbc.extservice;

/**
 * EmailAddress class holding information about an email address.
 *
 * @author  
 * @version $Version$
 */
public class EmailAddress {
    /** 
     * Creates new EmailAddress 
     * 
     */
    public EmailAddress() {
        _Address = "";
        _Name = "";
    }

    /** 
     * Creates new EmailAddress 
     * 
     * @param   address     The email address.
     * @param   name        The name.
     */
    public EmailAddress(final String address, final String name) {
        _Address = address;
        _Name = name;
		  if(_Name.length() == 1)
		  {
            _Name = _Name + " ";
		  }
    }

    private String _Address;
        
    /**
     * Get the email address.
     * 
     * @return  The email address.
     */
    public String getAddress() {
        return _Address;
    }
    
    /**
     * Set the email address.
     * 
     * @param   val  The email address.
     */
    public void setAddress(final String val) {
        _Address = val;
    }

    private String _Name;
    
    /**
     * Get the name.
     * 
     * @return  The name.
     */
    public String getName() {
        return _Name;
    }

    /**
     * Set the name.
     * 
     * @param   val  The name.
     */
    public void setName(final String val) {
        _Name = val;
        if(_Name.length() == 1)
        {
			  _Name = _Name + " ";
		  }
    }
    
    /**
     * Resets this instance of EmailAddress.
     */
    protected void reset() {
        _Name = "";
        _Address = "";        
    }
}
