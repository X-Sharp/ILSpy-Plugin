USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows
USING System.Windows.Controls
USING System.Windows.Data
USING System.Windows.Documents
USING System.Windows.Input
USING System.Windows.Media
USING System.Windows.Media.Imaging
USING System.Windows.Navigation
USING System.Windows.Shapes
USING ICSharpCode.ILSpy
USING ICSharpCode.ILSpy.Options
USING System.ComponentModel
USING System.Xml.Linq

BEGIN NAMESPACE ILSpy.XSharpLanguage

    /// <summary>
    /// Interaction logic for XSharpOptionPage.xaml
    /// </summary>
    [ExportOptionPage(Title := "XSharp")];
    PUBLIC PARTIAL CLASS XSharpOptionPage INHERIT UserControl IMPLEMENTS IOptionPage
    
        STATIC INITONLY ns := "XSharp" AS STRING
        
        #region Access Settings
        
        PUBLIC STATIC PROPERTY CurrentXSharpSettings AS XSharpOptions
            GET
                RETURN LoadXSharpSettings( ILSpySettings.Load())
            END GET
        END PROPERTY
        
        PUBLIC STATIC METHOD LoadXSharpSettings( settings AS ILSpySettings) AS XSharpOptions
            LOCAL xsSettings AS XSharpOptions
            LOCAL elt AS XElement
            //
            elt := settings[ns + "Options"]
            xsSettings := XSharpOptions{ elt }
            //
            RETURN xsSettings
            
            #endregion
            
        PUBLIC CONSTRUCTOR() //XSharpOptionPage()
            InitializeComponent()
            RETURN
            
            
        PUBLIC METHOD Load( settings AS ILSpySettings ) AS VOID
            // For loading options, use ILSpySetting's indexer.
            // If the specified section does exist, the indexer will return a new empty element.
            LOCAL e := settings[ns + "Options"] AS XElement
            // Now load the options from the XML document:
            LOCAL s := XSharpOptions{ e } AS XSharpOptions
            //
            SELF:DataContext := s
            
            
        PUBLIC METHOD Save( root AS XElement ) AS VOID
            LOCAL s := (XSharpOptions)SELF:DataContext AS XSharpOptions
            // Save the options back into XML:
            LOCAL section := XElement{ns + "Options"} AS XElement
            section:SetAttributeValue("upperkeyword", s:UpperKeyword)
			section:SetAttributeValue("ifstatement", s:IfStatement)
            // Replace the existing section in the settings file, or add a new section,
            // if required.
            LOCAL existingElement := root:Element(ns + "Options") AS XElement
            IF (existingElement != NULL)
                existingElement:ReplaceWith(section)
            ELSE
                root.Add(section)
            ENDIF
            
            END CLASS
            
            
    PUBLIC CLASS XSharpOptions INHERIT INotifyPropertyChanged
    
        PRIVATE _upperkeyword AS LOGIC
		PRIVATE _ifstatement AS LOGIC
        
        PUBLIC CONSTRUCTOR()
            SELF:_upperkeyword := TRUE
			SELF:_ifstatement = TRUE
            
        PUBLIC CONSTRUCTOR( e AS XElement )
            LOCAL attr AS XAttribute
            //
            attr := e:Attribute("upperkeyword")
            IF ( attr != NULL )
                LOCAL val := attr:Value AS STRING
                LOCAL bVal := TRUE AS LOGIC
                IF Boolean.TryParse( val, bVal )
                    SELF:_upperkeyword := bVal
                ELSE
                    SELF:_upperkeyword := TRUE
                ENDIF
            ENDIF
            //
            attr := e:Attribute("ifstatement")
            IF ( attr != NULL )
                LOCAL val := attr:Value AS STRING
                LOCAL bVal := TRUE AS LOGIC
                IF Boolean.TryParse( val, bVal )
                    SELF:_ifstatement := bVal
                ELSE
                    SELF:_ifstatement := TRUE
                ENDIF
            ENDIF
            //			
            
            
        PUBLIC PROPERTY UpperKeyword AS LOGIC
            GET
            RETURN _upperkeyword
            END GET
            
            SET 
                IF (_upperkeyword != VALUE) 
                    _upperkeyword := VALUE
                    OnPropertyChanged("UpperKeyword")
                ENDIF
            END SET
		END PROPERTY
		
        PUBLIC PROPERTY IfStatement AS LOGIC
            GET
            RETURN _ifstatement
            END GET
            
            SET 
                IF (_ifstatement != VALUE) 
                    _ifstatement := VALUE
                    OnPropertyChanged("IfStatement")
                ENDIF
            END SET
        END PROPERTY		
        
        PUBLIC EVENT PropertyChanged AS PropertyChangedEventHandler
        
    PROTECTED VIRTUAL METHOD OnPropertyChanged( propertyName AS STRING ) AS VOID
        IF (PropertyChanged != NULL) 
            PropertyChanged(SELF, PropertyChangedEventArgs{propertyName})
        ENDIF
        
    END CLASS
    
END NAMESPACE
