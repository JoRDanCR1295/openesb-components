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
 * @(#)BPELVisitorSupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.common.visitor;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.FaultHandlers;
import com.sun.bpel.model.Invoke;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.Sequence;
import com.sun.bpel.model.common.EInsightModelException;

import java.text.NumberFormat;
import java.util.Map;

/**
 * A base class for the BPEL visitor support.
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class BPELVisitorSupport {
    
    /** boolean System property that governs whether a invoke should be expanded if it has
     * inline fault and/or compensation handlers.
     */
    public static final String EXPAND_INVOKE_KEY =
        "com.sun.bpel.model.common.visitor.BPELVisitorSupport.expandInvoke";
    
    /** X coordinate presentation key */
    public static final String XLOC = "XLoc";
    
    /** Y coordinate presentation key */
    public static final String YLOC = "YLoc";
    
    /** Compensate handler delta X shift */
    private static final double COMPENSATE_DELTA_X = 119.0;
    
    /** Compensate / Fault handler initial delta Y shift */
    private static final double DELTA_Y = 200.0;
    
    /** Fault handler base delta X shift */
    private static final double FAULT_DELTA_X = 200.0;
    
    /** Fault handler radius */
    private static final double FAULT_RADIUS = 50.0;
    
    /** Invoke delta X shift */
    private static final double INVOKE_DELTA_X = 106.0;
    
    /** Invoke delta Y shift */
    private static final double INVOKE_DELTA_Y = 53.0;
    
    /** Holds number formatter */
    private static NumberFormat numberFormatter = null;
    
    /** Creates a new instance of BPELVisitorSupport */
    public BPELVisitorSupport() {
        super();
    }
    
    /** Convert invokes with inline fault and/or compensation handlers to an explicit
     * scope; transferring said handlers to the scope and making the invoke the sole activity
     * (within a sequence of course).  Conversion subject to System boolean property key
     * "com.sun.bpel.model.common.visitor.BPELVisitorSupport.expandInvoke".  Note, if latter not
     * present, it's assumed to be <code>true</code>.
     *
     * @param   invoke  Invoke element to potentially convert.
     * @return  Equivalent scope element or <code>null</code> if conversion is not necessary.
     *          Note, original invoke element is modified if conversion occurs.
     */
    public Scope expandInvoke(Invoke invoke) {
        Scope convScope = null;
        if (Boolean.valueOf(System.getProperty(EXPAND_INVOKE_KEY, "true")).booleanValue()
                && ((invoke.getCompensationHandler() != null) || (invoke.getCatchSize() > 0)
                    || (invoke.getCatchAll() != null))) {
            if (invoke.getOwnerDocument() == null) {
                throw new EInsightModelException("Associate element with owner XML document first!");
            }
            convScope = ((BPELDocument) invoke.getOwnerDocument()).createScope();
            if (!ValidateSupport.isAttributeAbsent(invoke.getName())) {
                convScope.setName(invoke.getName());
            }
            
            // Transfer the compensation handler
            if (invoke.getCompensationHandler() != null) {
                convScope.setCompensationHandler(invoke.getCompensationHandler());
                invoke.setCompensationHandler(null);
            }
            
            // Transfer the catch 
            FaultHandlers fhs = null;
            if (invoke.getCatchSize() > 0) {
                fhs = ((BPELDocument) invoke.getOwnerDocument()).createFaultHandlers();
                convScope.setFaultHandlers(fhs);
                while (invoke.getCatchSize() > 0) {
                    fhs.addCatch(invoke.getCatch(0));
                    invoke.removeCatch(0);
                }
            }
            
            // Transfer the catchAll
            if (invoke.getCatchAll() != null) {
                if (null == fhs) {
                    fhs = ((BPELDocument) invoke.getOwnerDocument()).createFaultHandlers();
                    convScope.setFaultHandlers(fhs);
                }
                fhs.setCatchAll(invoke.getCatchAll());
                invoke.setCatchAll(null);
            }
            
            // Set the presentation map
            Map pm = invoke.getPresentationMap();
            if (pm != null) {
                String xLoc = (String) pm.get(XLOC);
                String yLoc = (String) pm.get(YLOC);
                if ((xLoc != null) && (yLoc != null)) {
                    
                    // Position the scope
                    convScope.setPresentationMap(pm);
                    
                    double scopeX = Double.parseDouble(xLoc);
                    double scopeY = Double.parseDouble(yLoc);
                    
                    // Position the compensationHandler
                    if (convScope.getCompensationHandler() != null) {
                        double x = scopeX + COMPENSATE_DELTA_X;
                        double y = scopeY + DELTA_Y;
                        pm.put(XLOC, format(x));
                        pm.put(YLOC, format(y));
                        convScope.getCompensationHandler().setPresentationMap(pm);
                    }
                    
                    // Position the faultHandlers
                    if (fhs != null) {
                        double angle = 0;
                        int n = fhs.getCatchSize() + (fhs.getCatchAll() != null ? 1 : 0);
                        double angleDelta = 0;
                        if (n > 1) {
                            angleDelta = Math.PI / (2.0 * (n - 1));
                        }
                        
                        // Position the catch
                        for (int i = 0; i < fhs.getCatchSize(); i++) {
                            double x = scopeX + FAULT_DELTA_X + (FAULT_RADIUS * Math.cos(angle));
                            double y = scopeY + DELTA_Y + (FAULT_RADIUS * Math.sin(angle));
                            pm.put(XLOC, format(x));
                            pm.put(YLOC, format(y));
                            fhs.getCatch(i).setPresentationMap(pm);
                            angle += angleDelta;
                        }
                        
                        // Position the catchAll
                        if (fhs.getCatchAll() != null) {
                            double x = scopeX + FAULT_DELTA_X + (FAULT_RADIUS * Math.cos(angle));
                            double y = scopeY + DELTA_Y + (FAULT_RADIUS * Math.sin(angle));
                            pm.put(XLOC, format(x));
                            pm.put(YLOC, format(y));
                            fhs.getCatchAll().setPresentationMap(pm);
                        }
                    }
                    
                    // Reposition the invoke
                    double x = scopeX + INVOKE_DELTA_X;
                    double y = scopeY + INVOKE_DELTA_Y;
                    pm.put(XLOC, format(x));
                    pm.put(YLOC, format(y));
                    invoke.setPresentationMap(pm);
                }
            }
            
            Sequence seq = ((BPELDocument) invoke.getOwnerDocument()).createSequence();
            seq.addActivity(invoke);
            convScope.setActivity(seq);
        }
        return convScope;
    }
    
    /** Formats a double.
     * @param   d   double number to format.
     */
    private String format(double d) {
        if (null == numberFormatter) {
            numberFormatter = NumberFormat.getInstance();
            numberFormatter.setMinimumFractionDigits(1);
            numberFormatter.setMaximumFractionDigits(1);
        }
        return numberFormatter.format(d);
    }
}
