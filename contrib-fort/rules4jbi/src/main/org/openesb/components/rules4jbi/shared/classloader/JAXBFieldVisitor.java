/*
 * @(#)JAXBFieldVisitor.java        $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.shared.classloader;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Attribute;
import org.objectweb.asm.FieldVisitor;

/**
 * A field visitor that annotates the visiting field with the <code>@XmlElement</code>
 * annotation if it did not previously contain one. It is designed to work with
 * {@link org.openesb.components.rules4jbi.shared.classloader.JAXBClassAdapter JAXBClassAdapter}.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
 * 
 * @see org.openesb.components.rules4jbi.shared.classloader.JAXBClassAdapter
 * @since 0.4
 */
final class JAXBFieldVisitor implements FieldVisitor {
    
    private final FieldVisitor fv;
    
    private final String namespace;
    
    private final boolean required;

    private boolean isJAXBAnnotationPresent = false;
    
    public JAXBFieldVisitor(FieldVisitor fv, String namespace, boolean required) {
        this.fv = fv;
        this.namespace = namespace;
        this.required = required;
    }

    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
        if (visible && (Descriptor.XML_ELEMENT.equals(desc) || Descriptor.XML_ATTRIBUTE.equals(desc))) {
            isJAXBAnnotationPresent = true;
        }
        
        return fv.visitAnnotation(desc, visible);
    }

    public void visitAttribute(Attribute attr) {
        fv.visitAttribute(attr);
    }

    public void visitEnd() {
        if (!isJAXBAnnotationPresent) {
            AnnotationVisitor av = fv.visitAnnotation(Descriptor.XML_ELEMENT, true);

            if (av != null) {
                if (required) {
                    av.visit("required", Boolean.TRUE);
                }
                
                av.visit("namespace", namespace);
                av.visitEnd();
            }
        }
        
        fv.visitEnd();
    }
}
