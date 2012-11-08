
This test is designed to test the issue reported by open-jbi-components issue 108 (https://open-jbi-components.dev.java.net/issues/show_bug.cgi?id=108).

Issue description: When two or more services from StrikeIron are used in a single application deployment caused problem in loading the application.  Basically duplicate global complex types are defined in the inline schema of two different WSDLs, which caused a duplicate global types exception being thrown from XmlBeans.  The fix is modifying XmlBeans to ignore duplicates global components but log the situation as warnings.