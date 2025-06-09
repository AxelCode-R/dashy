




document.addEventListener('DOMContentLoaded', function() {
  const container = document.getElementById('logger_container');

  const observer = new MutationObserver(function(mutationsList, observer) {
    mutationsList.forEach(mutation => {
      if (mutation.type === 'childList') {
        var controlbar_icon = document.getElementById('controlbar_icon');
        controlbar_icon.classList.remove('icon-animation');
        void controlbar_icon.offsetWidth;
        controlbar_icon.classList.add('icon-animation');

        const elements = document.querySelectorAll('#logger_container .logger_element');
        if (elements.length > 10) {
          for (var i = 0; i < elements.length - 10; i++) {
            elements[i].remove();
          }
        }
        mutation.addedNodes.forEach(node => {
          if (node.nodeType === Node.ELEMENT_NODE) {
            const node_element_text = node.querySelector('.logger_element_text');
            node_element_text.addEventListener('click', function() {
              node_element_text.classList.toggle('logger_element_text_ellapsed');
            });
          }
        });
      }
    });
  });

  observer.observe(container, { childList: true });
});
