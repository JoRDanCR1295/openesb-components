 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.component;

import java.util.LinkedList;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;

public class TransformerPool {
    private static final TransformerFactory cTransformerFact =
            TransformerFactory.newInstance();
    
    private final LinkedList<Transformer> mTransformers;

    /** Creates a new instance of TransformerPool */
    public TransformerPool() {
        mTransformers = new LinkedList<Transformer>();
    }
    
    public TransformerPool(int size) throws TransformerConfigurationException {
        this();
        for (int i = 0; i < size; ++i) {
            mTransformers.addFirst(cTransformerFact.newTransformer());
        }
    }
    
    public Transformer retrieve() throws TransformerConfigurationException {
        Transformer transformer = null;
        
        synchronized(this) {
            if (!mTransformers.isEmpty()) {
                transformer = mTransformers.removeFirst();
            } else {
                transformer = cTransformerFact.newTransformer();
            }
        }
        return transformer;
    }
    
    public boolean relinquish(Transformer transformer) {
        boolean success = false;
        if (transformer != null) {
            synchronized (this) {
                if (!mTransformers.contains(transformer)) {
                    mTransformers.addFirst(transformer);
                    success = true;
                }
            }
        }
        return success;
    }
}
