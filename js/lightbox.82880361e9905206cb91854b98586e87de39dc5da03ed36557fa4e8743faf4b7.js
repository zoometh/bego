(() => {
  // <stdin>
  document.addEventListener("DOMContentLoaded", () => {
    document.querySelectorAll("a.lightbox").forEach((link) => {
      link.addEventListener("click", (e) => {
        e.preventDefault();
        const overlay = document.createElement("div");
        overlay.className = "lightbox-overlay";
        const img = document.createElement("img");
        img.src = link.href;
        overlay.appendChild(img);
        document.body.appendChild(overlay);
        overlay.addEventListener("click", () => {
          overlay.remove();
        });
      });
    });
  });
})();
//# sourceMappingURL=data:application/json;base64,ewogICJ2ZXJzaW9uIjogMywKICAic291cmNlcyI6IFsiPHN0ZGluPiJdLAogICJzb3VyY2VzQ29udGVudCI6IFsiZG9jdW1lbnQuYWRkRXZlbnRMaXN0ZW5lcignRE9NQ29udGVudExvYWRlZCcsICgpID0+IHtcclxuICBkb2N1bWVudC5xdWVyeVNlbGVjdG9yQWxsKCdhLmxpZ2h0Ym94JykuZm9yRWFjaChsaW5rID0+IHtcclxuICAgIGxpbmsuYWRkRXZlbnRMaXN0ZW5lcignY2xpY2snLCBlID0+IHtcclxuICAgICAgZS5wcmV2ZW50RGVmYXVsdCgpO1xyXG5cclxuICAgICAgY29uc3Qgb3ZlcmxheSA9IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ2RpdicpO1xyXG4gICAgICBvdmVybGF5LmNsYXNzTmFtZSA9ICdsaWdodGJveC1vdmVybGF5JztcclxuXHJcbiAgICAgIGNvbnN0IGltZyA9IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoJ2ltZycpO1xyXG4gICAgICBpbWcuc3JjID0gbGluay5ocmVmO1xyXG5cclxuICAgICAgb3ZlcmxheS5hcHBlbmRDaGlsZChpbWcpO1xyXG4gICAgICBkb2N1bWVudC5ib2R5LmFwcGVuZENoaWxkKG92ZXJsYXkpO1xyXG5cclxuICAgICAgb3ZlcmxheS5hZGRFdmVudExpc3RlbmVyKCdjbGljaycsICgpID0+IHtcclxuICAgICAgICBvdmVybGF5LnJlbW92ZSgpO1xyXG4gICAgICB9KTtcclxuICAgIH0pO1xyXG4gIH0pO1xyXG59KTtcclxuIl0sCiAgIm1hcHBpbmdzIjogIjs7QUFBQSxXQUFTLGlCQUFpQixvQkFBb0IsTUFBTTtBQUNsRCxhQUFTLGlCQUFpQixZQUFZLEVBQUUsUUFBUSxVQUFRO0FBQ3RELFdBQUssaUJBQWlCLFNBQVMsT0FBSztBQUNsQyxVQUFFLGVBQWU7QUFFakIsY0FBTSxVQUFVLFNBQVMsY0FBYyxLQUFLO0FBQzVDLGdCQUFRLFlBQVk7QUFFcEIsY0FBTSxNQUFNLFNBQVMsY0FBYyxLQUFLO0FBQ3hDLFlBQUksTUFBTSxLQUFLO0FBRWYsZ0JBQVEsWUFBWSxHQUFHO0FBQ3ZCLGlCQUFTLEtBQUssWUFBWSxPQUFPO0FBRWpDLGdCQUFRLGlCQUFpQixTQUFTLE1BQU07QUFDdEMsa0JBQVEsT0FBTztBQUFBLFFBQ2pCLENBQUM7QUFBQSxNQUNILENBQUM7QUFBQSxJQUNILENBQUM7QUFBQSxFQUNILENBQUM7IiwKICAibmFtZXMiOiBbXQp9Cg==
