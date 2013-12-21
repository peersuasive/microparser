/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2013 - Raw Material Software Ltd.

   Permission is granted to use this software under the terms of either:
   a) the GPL v2 (or any later version)
   b) the Affero GPL v3

   Details of these licenses can be found at: www.gnu.org/licenses

   JUCE is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

   ------------------------------------------------------------------------------

   To release a closed-source product which uses JUCE, commercial licenses are
   available: visit www.juce.com for more information.

  ==============================================================================
*/

#ifndef JUCE_COMPONENT_H_INCLUDED
#define JUCE_COMPONENT_H_INCLUDED

//==============================================================================
/**
    The base class for all JUCE user-interface objects.
*/

class JUCE_API Component : public MouseListener
{
public:
    //==============================================================================
    /** Creates a component.

        To get it to actually appear, you'll also need to:
        - Either add it to a parent component or use the addToDesktop() method to
          make it a desktop window
        - Set its size and position to something sensible
        - Use setVisible() to make it visible

        And for it to serve any useful purpose, you'll need to write a
        subclass of Component or use one of the other types of component from
        the library.
    */

    Component();
    virtual ~Component();
    explicit Component (const String& componentName);
    const String& getName() const noexcept { return componentName; }
    virtual void setName (const String& newName);
    const String& getComponentID() const noexcept { return componentID; }
    void setComponentID (const String& newID);
    virtual void setVisible (bool shouldBeVisible);
    bool isVisible() const noexcept { return flags.visibleFlag; }
    virtual void visibilityChanged();
    bool isShowing() const;
    virtual void addToDesktop (int windowStyleFlags,
                               void* nativeWindowToAttachTo = nullptr);
    void removeFromDesktop();
    bool isOnDesktop() const noexcept;
    ComponentPeer* getPeer() const;
    virtual void userTriedToCloseWindow();
    virtual void minimisationStateChanged (bool isNowMinimised);
    virtual float getDesktopScaleFactor() const;
    void toFront (bool shouldAlsoGainFocus);
    void toBack();
    void toBehind (Component* other);
    void setAlwaysOnTop (bool shouldStayOnTop);
    bool isAlwaysOnTop() const noexcept;

    int getRight() const noexcept { return bounds.getRight(); }

    Point<int> getPosition() const noexcept { return bounds.getPosition(); }

    const Rectangle<int>& getBounds() const noexcept { return bounds; }

    Rectangle<int> getLocalBounds() const noexcept;
    
    void getVisibleArea (RectangleList<int>& result, bool includeSiblings) const;
    
    int getScreenY() const;
    Point<int> getScreenPosition() const;
   
    Point<int> getLocalPoint (const Component* sourceComponent,
                              Point<int> pointRelativeToSourceComponent) const;

    Rectangle<int> getLocalArea (const Component* sourceComponent,
                                 const Rectangle<int>& areaRelativeToSourceComponent) const;
    Point<int> localPointToGlobal (Point<int> localPoint) const;

    void setBounds (int x, int y, int width, int height);
    void setBounds (const Rectangle<int>& newBounds);
    void setBounds (const RelativeRectangle& newBounds);

    void setBoundsRelative (float proportionalX, float proportionalY,
                            float proportionalWidth, float proportionalHeight);

    void setBoundsToFit (int x, int y, int width, int height,
                         Justification justification,
                         bool onlyReduceInSize);

    Component* getChildComponent (int index) const noexcept;

    void addChildComponent (Component* child, int zOrder = -1);

    template <class TargetClass>
    TargetClass* findParentComponentOfClass() const
    {
        for (Component* p = parentComponent; p != nullptr; p = p->parentComponent)
            if (TargetClass* const target = dynamic_cast <TargetClass*> (p))
                return target;
        return nullptr;
    }

    virtual void childrenChanged();

    virtual bool hitTest (int x, int y);

    void repaint();

    Image createComponentSnapshot (const Rectangle<int>& areaToGrab,
                                   bool clipImageToComponentBounds = true,
                                   float scaleFactor = 1.0f);

    void paintEntireComponent (Graphics& context, bool ignoreAlphaLevel);
    
    LookAndFeel& getLookAndFeel() const noexcept;

    virtual void lookAndFeelChanged();

    static Component* JUCE_CALLTYPE getCurrentlyFocusedComponent() noexcept;
    
    virtual KeyboardFocusTraverser* createFocusTraverser();

    virtual void mouseDrag (const MouseEvent& event) override;

    enum FocusChangeType
    {
        focusChangedByMouseClick,
        focusChangedByTabKey,
        focusChangedDirectly
    };
    
    Point<int> getMouseXYRelative() const;

    void enterModalState (bool takeKeyboardFocus = true,
                          ModalComponentManager::Callback* callback = nullptr,
                          bool deleteWhenDismissed = false);
    
    static Component* JUCE_CALLTYPE getCurrentlyModalComponent (int index = 0) noexcept;
    
    template <class ComponentType>
    class SafePointer
    {
    public:
        SafePointer() noexcept {}
        SafePointer (ComponentType* const component) : weakRef (component) {}
        SafePointer (const SafePointer& other) noexcept : weakRef (other.weakRef) {}
        SafePointer& operator= (const SafePointer& other) { weakRef = other.weakRef; return *this; }
        SafePointer& operator= (ComponentType* const newComponent) { weakRef = newComponent; return *this; }
        ComponentType* getComponent() const noexcept { return dynamic_cast <ComponentType*> (weakRef.get()); }
        operator ComponentType*() const noexcept { return getComponent(); }
        ComponentType* operator->() noexcept { return getComponent(); }
        const ComponentType* operator->() const noexcept { return getComponent(); }
        void deleteAndZero() { delete getComponent(); }
        bool operator== (ComponentType* component) const noexcept { return weakRef == component; }
        bool operator!= (ComponentType* component) const noexcept { return weakRef != component; }
    private:
        WeakReference<Component> weakRef;
    };

    class JUCE_API BailOutChecker
    {
    public:
        BailOutChecker (Component* component);
        bool shouldBailOut() const noexcept;
    private:
        const WeakReference<Component> safePointer;
        JUCE_DECLARE_NON_COPYABLE (BailOutChecker)
    };
    class JUCE_API Positioner
    {
    public:
        explicit Positioner (Component& component) noexcept;
        virtual ~Positioner() {}
        Component& getComponent() const noexcept { return component; }
        virtual void applyNewBounds (const Rectangle<int>& newBounds) = 0;
    private:
        Component& component;
        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Positioner)
    };
    
    JUCE_DEPRECATED (Point<int> relativePositionToOtherComponent (const Component*, Point<int>) const);
private:
    friend class ComponentPeer;
    
    ScopedPointer <CachedComponentImage> cachedImage;
    class MouseListenerList;

    friend struct ContainerDeletePolicy<MouseListenerList>;

    ScopedPointer <Array <KeyListener*> > keyListeners;
    
    NamedValueSet properties;
   
    WeakReference<Component>::Master masterReference;

    struct ComponentFlags
    {
        bool hasHeavyweightPeerFlag : 1;
        bool visibleFlag : 1;
        bool opaqueFlag : 1;
        bool ignoresMouseClicksFlag : 1;
        bool allowChildMouseClicksFlag : 1;
        bool wantsFocusFlag : 1;
        bool isFocusContainerFlag : 1;
        bool dontFocusOnMouseClickFlag : 1;
        bool alwaysOnTopFlag : 1;
        bool bufferToImageFlag : 1;
        bool bringToFrontOnClickFlag : 1;
        bool repaintOnMouseActivityFlag : 1;
        bool currentlyModalFlag : 1;
        bool isDisabledFlag : 1;
        bool childCompFocusedFlag : 1;
        bool dontClipGraphicsFlag : 1;
        bool mouseDownWasBlocked : 1;
    };

    union
    {
        uint32 componentFlags;
        ComponentFlags flags;
    };

    uint8 componentTransparency;
    void internalMouseEnter (MouseInputSource, Point<int>, Time);

    void internalBroughtToFront();
    
    void internalChildFocusChange (FocusChangeType, const WeakReference<Component>&);
    
    void internalRepaintUnchecked (const Rectangle<int>&, bool);
    
    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Component)
protected:
    virtual ComponentPeer* createNewPeer (int styleFlags, void* nativeWindowToAttachTo);
};

#endif   // JUCE_COMPONENT_H_INCLUDED

