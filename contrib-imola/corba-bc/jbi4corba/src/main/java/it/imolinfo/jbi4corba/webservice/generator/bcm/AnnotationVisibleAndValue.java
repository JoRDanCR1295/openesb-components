 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator.bcm;

import java.util.List;

import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * XXX javadoc.
 */
public class AnnotationVisibleAndValue {
    public Boolean visible;
    public List<AnnotationValue> value;

    /**
     * 
     * @param visible  The visible
     * @param value    The value
     */
    public AnnotationVisibleAndValue(Boolean visible,
                                     List<AnnotationValue> value) {
        this.visible = visible;
        this.value   = value;
    }

    /**
     * @return  The return
     */
    public String toString() {
      return ReflectionToStringBuilder.toString(this);
    }
}
