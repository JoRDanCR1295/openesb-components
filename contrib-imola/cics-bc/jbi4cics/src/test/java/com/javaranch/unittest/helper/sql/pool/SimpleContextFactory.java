package com.javaranch.unittest.helper.sql.pool;


import java.util.Hashtable;

import javax.naming.Context;
import javax.naming.spi.InitialContextFactory;

/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
public class SimpleContextFactory implements InitialContextFactory
{

    private static SimpleContext instance;

    /**
     * Method getInitialContext Returns the SimpleContext for use.
     *
     *
     * @param environment
     *
     * @return
     *
     */
    public Context getInitialContext( Hashtable environment )
    {

        if ( instance == null )
        {
            instance = new SimpleContext();
        }
        return instance;
    }
}
