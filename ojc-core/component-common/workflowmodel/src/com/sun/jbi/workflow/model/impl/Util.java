/*
 * Util.java
 * 
 * Created on Jun 27, 2007, 3:28:06 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.workflow.model.impl;

import java.util.List;
import java.util.Map;

import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.ModelException;
import com.sun.jbi.workflow.model.Notification;
import com.sun.jbi.workflow.model.OnePartContainer;
import com.sun.jbi.workflow.model.PortTypeModelElement;
import com.sun.jbi.workflow.model.Task;

/**
 * 
 * @author radval
 */
public class Util {

    public static Notification findNotification(String qname,
            ModelElement element) {
        Notification notification = null;
        Task task = findTaskAncestor(element);
        if (task != null) {
            notification = task.getNotification(qname);
        }
        return notification;
    }

    public static Task findTaskAncestor(ModelElement element) {
        Task task = null;
        if (element != null) {
            ModelElement el = element;
            while (el != null) {
                if (el instanceof Task) {
                    task = (Task) el;
                    break;
                } else {
                    el = el.getParent();
                }
            }
        }

        return task;
    }

    public static Task findTasksAncestor(ModelElement element) {
        Task task = null;
        if (element != null) {
            ModelElement el = element;
            while (el != null) {
                if (el instanceof Task) {
                    task = (Task) el;
                    break;
                } else {
                    el = el.getParent();
                }
            }
        }

        return task;
    }

    public static Operation getWSDLOperation(PortType pt, String operationName,
            ModelElement element) throws ModelException {
        if (operationName == null) {
            return null;
        }
        
        List<Operation> allOpts = pt.getOperations();
        for (Operation opt : allOpts) {
            if (opt.getName().equals(operationName))  {
                return opt;
            }
        }
        return null; 
    }

    public static PortType getWSDLPortType(
            QName portTypeQname, ModelElement element)
            throws ModelException {
        PortType portType = null;
        if (portTypeQname == null) {
            return null;
        }

        Task task = findTasksAncestor(element);
        if (task == null) {
            return null;
        }
        List<Definition> allImports = task.getImportWSDLs();
        if (allImports != null && allImports.size() > 0) {
            for (int i = 0; i < allImports.size(); i++) {
                Definition wsdlDef = allImports.get(i);
                Map portTypes = wsdlDef.getPortTypes();
                if (portTypes != null
                        && portTypes.size() > 0
                        && portTypes.containsKey(portTypeQname)) {
                    portType = (PortType) portTypes
                            .get(portTypeQname);
                    break;
                }
            }
        }
        return portType;
    }

    public static Definition getWSDLDefinition(Task task)
            throws ModelException {
        List<Definition> allImports = task.getImportWSDLs();
        PortType portType = task.getPortType();
        Operation taskOp = task.getWSDLOperation();
        Definition result = null;

        if (allImports != null && allImports.size() > 0) {
            for (int i = 0; i < allImports.size(); i++) {
                Definition wsdlDef = allImports.get(i);
                PortType pt = wsdlDef.getPortType(portType.getQName());
                if (pt != null) {
                    List<Operation> opts = pt.getOperations();
                    for (Operation op : opts) {
                        if (op.getName().equals(taskOp.getName())) {
                            result = wsdlDef;
                            break;
                        }
                    }
                }

            }

        }
        return result;
    }

    public static PortType getPortType(QName portType,
            ModelElement element) throws ModelException {
        PortType wsdlPortType = getWSDLPortType(
                portType, element);
        return wsdlPortType;
    }

    public static Part getPart(OnePartContainer element) throws ModelException {
        Part part = null;
        if (element != null) {
            String partName = element.getPart();
            PortTypeModelElement n = element.getPortTypeElement();
             Operation operation = n.getWSDLOperation();
                if (operation != null) {
                    Input input = operation.getInput();
                    if (input != null) {
                        Message message = input.getMessage();
                        if (message != null) {
                            part = message.getPart(partName);
                        }
                    }
                }
            }
        return part;
    }

    public static String getTargetNamesapce(ModelElement element) {
        String ns = null;

        Task task = findTasksAncestor(element);
        if (task != null) {
            ns = task.getTargetNamespace();
        }

        return ns;
    }

}
