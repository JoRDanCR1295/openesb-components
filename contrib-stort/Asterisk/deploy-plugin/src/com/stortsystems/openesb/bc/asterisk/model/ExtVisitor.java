/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: ExtVisitor.java,v 1.1 2008/01/20 16:40:07 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.model;

public interface ExtVisitor {
    void visit(BindingExt bindingExt);
    void visit(OperationExt operationExt);
    void visit(InputExt inputExt);
    void visit(OutputExt outputExt);
    void visit(FaultExt faultExt);
    void visit(PortExt portExt);
}
