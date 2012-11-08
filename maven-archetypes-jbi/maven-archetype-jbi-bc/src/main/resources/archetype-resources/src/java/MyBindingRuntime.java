/*
 * MyBindingRuntime.java
 */

package ${package};

import com.sun.jbi.sample.component.common.ComponentImpl;
import javax.jbi.component.ComponentLifeCycle;

/**
 * This class extends the ComponentImpl that implements javax.jbi.component.Component
 * interface required for the component contract at runtime.
 * 
 * This class provides the MyBinding specific ComponentLifeCycle implementation 
 * by creating the MyBindingComponentLifeCycle class in the createComponentLifeCycle
 * method by overriding the base ComponentImpl class implementation.
 *
 * Add component runtime specific functionality here.
 *
 * @see javax.jbi.component.Component
 * @see com.sun.jbi.sample.component.common.ComponentImpl
 * @see com.sun.jbi.sample.binding.MyBindingComponentLifeCycle
 * @author chikkala
 */
public class MyBindingRuntime extends ComponentImpl {
    
    /** Creates a new instance of MyBindingRuntime */
    public MyBindingRuntime() {
        super();
    }
    /**
     * overriding the parent's createComponentLifeCycle to create 
     * MyBinding specific component lifecycle implementation.
     */
    protected ComponentLifeCycle createComponentLifeCycle() {
        return new MyBindingComponentLifeCycle();
    }
    
}
